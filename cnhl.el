;;; cnhl.el --- Make Chinese highlight lexically -*- lexical-binding: t -*-

;; Copyright (C) 2022 Rosario S.E.

;; Author: Rosario S.E. <ser3vau@gmail.com>
;; URL: https://github.com/3vau/cnhl

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; A Emacs trick to make Chinese highlight lexically.
;; For more infomation, read https://github.com/3vau/cnhl/blob/main/README.md
;; and https://emacs-china.org/t/topic/18977/38
;; Thanks to people who helped me:
;;  @LdBeth https://emacs-china.org/u/LdBeth
;;  @cireu https://emacs-china.org/u/cireu
;;  @twlz0ne https://emacs-china.org/u/twlz0ne

;;; Code:
;; 主题（默认适应暗色）
;; 红色系
(defface cnhl-n-r-j
  '((t (:foreground "#F0B0A5")))
  "名词、代词、简称颜色"
  :group 'cnhl)
;; 绿色系
(defface cnhl-v-i-id
  '((t (:foreground "#C8E3A1")))
  "动词、习语颜色"
  :group 'cnhl)
;; 橙黄色系
(defface cnhl-a
  '((t (:foreground "#EDC7A8")))
  "形容词颜色"
  :group 'cnhl)
(defface cnhl-f-s-t-m-q-mq
  '((t (:foreground "#EDBD9D")))
  "方位词、处所词、时间词、数词、量词、数量词颜色"
  :group 'cnhl)
;; 浅黄色系
(defface cnhl-d-c-p
  '((t (:foreground "#EDE390")))
  "副词、连词、介词颜色"
  :group 'cnhl)
;; 中黄色系
(defface cnhl-u-y-h-k
  '((t (:foreground "#D7C8A4")))
  "助词、语气助词、前接成分、后接成分颜色"
  :group 'cnhl)
;; 其它（thulac无法判断的词便会输出其它）
(defface cnhl-g-w-x-e-o
  '((t (:foreground "gray80")))
  "语素、标点、叹词、拟声词及其它颜色"
  :group 'cnhl)

(defvar cnhl--thulac-start-command
  (list
   "python3"
   "-c"
   "import sys;import json;import thulac;t=thulac.thulac();exec('while True:print(json.dumps(t.cut(sys.stdin.readline()),ensure_ascii=False))')")
  "thulac服务端代码")
;; 不知如何将循环和变量赋值放在一行里，故而使用exec

(defun cnhl--thulac-start ()
  "启动python版thulac词性分析process"
  (make-process
   :name "thulac"
   :buffer "*thulac*"
   :command cnhl--thulac-start-command)
  ;; 第一次实例化thulac时会耗费一段时间加载分析模型，加载完毕会输出"Model loaded succeed"作为提示
  ;; 使用accept-process-output等待加载完成。这段时间（大约数秒）emacs会卡住。
  (accept-process-output (get-process "thulac"))
  ;; 绑定filter
  (set-process-filter
   (get-process "thulac")
   'cnhl--thulac-filter)
  ;; 退出emacs时自动杀掉thulac process，避免弹出询问窗口
  (set-process-query-on-exit-flag (get-process "thulac") nil))

(defvar *cnhl-hl-enable* t
  "是否启用高亮，用于仅分词的情况")

(defvar *cnhl-last-info*
  (list :beg nil :end nil :result nil :buffer nil)
  "上一分析句的信息")

(defvar *cnhl-filter-timer* nil
  "用于临时存储filter所用的timer")

(defvar *cnhl-major-mode* (list 'org-mode 'text-mode 'org-journal-mode)
  "允许进行中文高亮的buffer mode")

(defvar *cnhl--whole-buffer* nil
  "全buffer高亮模式开关变量")

(defun cnhl--thulac-filter (process output)
  "接收python thulac传回的中文分析结果，进行初步处理后执行操作函数"
  (when (member major-mode *cnhl-major-mode*)
    ;; 使用python -c方式运行的process，单次输出较长的字符串时会被emacs分段读取传入filter（大约在第800C左右），
    ;; 执行python文件则无此问题。于是此处设置timer延迟执行，防止意外断句
    ;; 把输出记录下来
    (plist-put *cnhl-last-info* :result output)
    ;; 设置timer
    (setq *cnhl-filter-timer*
          (run-at-time
           ;; 默认间隔0.05s足矣
           "0.05" nil '(lambda ()
                         ;; 若开启高亮则执行
                         (when *cnhl-hl-enable*
                           (cnhl--run))
                         ;; 执行高亮后删掉自己
                         (setq *cnhl-filter-timer* nil)
                         ;; 若启动全buffer高亮，则自动跳到下一行重复执行高亮
                         (when *cnhl--whole-buffer*
                           (goto-line (+ (line-number-at-pos) 1))
                           (cnhl)
                           ;; 执行到buffer底部则退出全buffer高亮
                           (when (= (line-end-position) (buffer-end 1))
                             (setq *cnhl--whole-buffer* nil))))))))

(cl-defstruct (cnhl-word (:type vector))
  "thulac返回值的结构，word为词语本身，part为词性代码"
  word part)

(defun cnhl--current-word-info ()
  "获取光标所在词的一系列信息，用于中文分词"
  (interactive)
  ;; 若缓存中没有光标所在句的结果，则对当前句执行高亮以获得信息
  (unless (and (plist-get *cnhl-last-info* :result)
               (string= (buffer-name) (plist-get *cnhl-last-info* :buffer))
               (> (point) (plist-get *cnhl-last-info* :beg))
               (< (point) (plist-get *cnhl-last-info* :end)))
    (cnhl-sentence)
    ;; 等待分析结果出来，避免因为timer延时而导致的意外
    (accept-process-output (get-process "thulac")))
    ;; 从前往后将每个词的结束位置与光标位置相比较，当某词词末大于光标位置时，该位置即为光标所在词词末
    ;; array - 句子分析结果；p - 光标词词末；prev-1 - 上一词词末；prev-2 - 上上词词末
    (let ((array (json-parse-string
                  (plist-get *cnhl-last-info* :result)))
          (p (plist-get *cnhl-last-info* :beg))
          (prev-1 nil)
          (prev-2 nil))
      ;; idx - 在遍历的词的位置；word - 在遍历的词
      (cl-loop for idx = 0 then (1+ idx)
	       for word = (cnhl-word-word (aref array idx)) then (cnhl-word-word (aref array idx))
	       ;; 挨个腾地儿
	       do (setq prev-2 prev-1
			       prev-1 p
			       p (+ p (length word)))
	       ;; 当正在遍历的词末位置大于光标位置时（超过去了），返回本次词末、上词词末、上上词词末
	       when (> p (point)) do (cl-return (list :end p :prev-1 prev-1 :prev-2 prev-2)))
      ))

(defun cnhl-forward-word ()
  (interactive)
  (if (member major-mode *cnhl-major-mode*)
      ;; 前往本词词末，即下一词词首
      (goto-char (plist-get (cnhl--current-word-info) :end))
    ;; 若不在设置启用的major-mode中，则fallback回emacs默认的分词操作函数
    (forward-word)))

(defun cnhl-backward-word ()
  (interactive)
  (if (member major-mode *cnhl-major-mode*)
      ;; 光标在句首时，无法抓到上一句的标点，手动将光标前移
      (if (= (point) (line-beginning-position))
          (goto-char (- (point) 1))
        ;; 前往上上词词末，即上词词首
        (goto-char (plist-get (cnhl--current-word-info) :prev-2)))
    (backward-word)))

(defun cnhl-backward-kill-word ()
  (interactive)
  (if (member major-mode *cnhl-major-mode*)
      (progn (when (= (point) (line-beginning-position))
               (goto-char (- (point) 1)))
             (let ((result (cnhl--current-word-info)))
               ;; 如果光标位置在词末，删掉上上词词末至上词词末，即上词词首至本词词首，即上一个词
               (if (= (point) (plist-get result :prev-1))
                   (delete-region
                    (plist-get result :prev-2)
                    (plist-get result :prev-1))
                 ;; 否则整个删掉光标所在词
                 (delete-region
                  (plist-get result :prev-1)
                  (plist-get result :end)))))
    (backward-kill-word 1)))

(defun cnhl-kill-word ()
  (interactive)
  (if (member major-mode *cnhl-major-mode*)
      (let ((result (cnhl--current-word-info)))
        ;; 删掉上词词末至本词词末，即本词词首至下词词首，即本词
        (delete-region
         (plist-get result :prev-1)
         (plist-get result :end)))
    (kill-word 1)))

(defvar cnhl--face-items
  '(("n" 'cnhl-n-r-j)
    ("r" 'cnhl-n-r-j)
    ("j" 'cnhl-n-r-j)
    ("u" 'cnhl-u-y-h-k)
    ("y" 'cnhl-u-y-h-k)
    ("h" 'cnhl-u-y-h-k)
    ("k" 'cnhl-u-y-h-k)
    ("v" 'cnhl-v-i-id)
    ("i" 'cnhl-v-i-id)
    ("a" 'cnhl-a)
    ("d" 'cnhl-d-c-p)
    ("c" 'cnhl-d-c-p)
    ("p" 'cnhl-d-c-p)
    ("g" 'cnhl-g-w-x-e-o)
    ("w" 'cnhl-g-w-x-e-o)
    ("x" 'cnhl-g-w-x-e-o)
    ("e" 'cnhl-g-w-x-e-o)
    ("o" 'cnhl-g-w-x-e-o))
  "用于生成哈希表的对应关系list")
(defvar cnhl--face-table (make-hash-table :test 'equal) ;; 必须用equal判断
  "对应词性类型的第一个字母与类型的face的关系的哈希表")

(cl-loop for i in cnhl--face-items ;; 初始化（填充）哈希表
	 do (puthash (car i) (cdr i) cnhl--face-table))

(defun cnhl--get-face (str)
  "匹配词性类型对应的face"
  (or (gethash (string (aref str 0)) cnhl--face-table) 'cnhl-f-s-t-m-q-mq)) ;; 用首字母从哈希表中获取值

(defun cnhl--run ()
  "根据存储的信息执行中文词性高亮"
  (save-excursion
    ;; 清除高亮区域内已有的overlay
    (dolist (overlay (overlays-in (plist-get *cnhl-last-info* :beg)
                                  (1- (plist-get *cnhl-last-info* :end)))) ;; 减一避免清过一个词
      (delete-overlay overlay))
    ;; 光标移到句首
    (goto-char (plist-get *cnhl-last-info* :beg))
    ;; 设变量num，从0开始遍历所有分析出的词语
    (cl-loop for num = 0 then (1+ num)
	     with output = (json-parse-string (plist-get *cnhl-last-info* :result))
	     do (condition-case err
		    ;; 选取item为output数组中的第num个词的分析结果，格式为数组，第0项为词语，第1项为词性标识
		    (let* ((item (aref output num))
			   ;; 向前搜索item的第一项，即目标词语，记录词语后端位置，搜索范围仅限分析句内
			   (end (search-forward
				 (cnhl-word-word item)
				 (plist-get *cnhl-last-info* :end)))
			   ;; 根据搜索出的位置新建overlay
			   (overlay (make-overlay
				     (- end (length (aref item 0))) end)))
		      ;; 贴overlay的颜色
		      (overlay-put
		       overlay 'face
		       (cnhl--get-face (cnhl-word-part item))))
		  ;; 如果报错（如搜索出界），则return停止高亮
		  ;; 从emacs 27 转至emacs 29后，return改名为cl-return
		  (t (cl-return))))))

(defun cnhl (&optional beg end)
  "中文词性高亮"
  (interactive)
  ;; 启动thulac
  (unless (get-process "thulac") (cnhl--thulac-start))
  (when *cnhl-filter-timer*
    (cancel-timer *cnhl-filter-timer*)
    (setq *cnhl-filter-timer* nil))
  ;; 若无参数运行，则默认取当前行
  (unless beg
    (setq beg (line-beginning-position))
    (setq end (line-end-position)))
  ;; 记录字符串数据
  (setq *cnhl-last-info*
        (list :beg beg
              :end end
              :buffer (buffer-name)))
  ;; 将范围内的字符串送入thulac进行分析，结果返回至thulac-receive-output进行操作
  (process-send-string
   "thulac"
   (concat
    (string-trim (buffer-substring-no-properties beg end))
    "\n")))

(defun cnhl-buffer ()
  "对当前buffer全文逐行进行中文词性高亮"
  (interactive)
  ;; 启动全文高亮后移动光标至顶部，开始进行高亮
  (setq *cnhl--whole-buffer* t)
  (goto-char (point-min))
  (cnhl))

(defvar *cnhl-sentence-beginning-position* nil
  "记录本次语句高亮的起始点")

(defun cnhl-sentence ()
  "对当前句进行中文词性高亮"
  (interactive)
  (save-excursion
    ;; 生成句读匹配的正则，不能把group置于or的外侧，否则会导致在句末的高亮报错（可能与返回值有关）
    (let* ((regxp (rx (or (group "，")
                          (group "。")
                          (group "？")
                          (group "；")
                          (group "：")
                          (group "、")
                          (group "‘")
                          (group "’")
                          (group "“")
                          (group "”")
                          (group "…")
                          (group "！")
                          (group "（")
                          (group "）")
                          (group "～")
                          (group "*")
			  (group "《")
			  (group "》")
			  (group "『")
			  (group "』")
			  (group "〖")
			  (group "〗")
			  (group "「")
			  (group "」")
			  (group "【")
			  (group "】")
			  (group "〔")
			  (group "〕")
			  (group "［")
			  (group "］")
			  (group "｛")
			  (group "｝")
			  (group "—")
			  (group "｜")
			  (group "＊")
			  (group "·")
			  (group "・")
			  (group "￥")
			  (group "％"))))
           (end
            ;; 语段起始结束位置，若无标点则直到行末
	    ;; 本次编辑的起始点大于光标位置，则从起始点向后搜索作为高亮终点
	    ;; 避免一次性大量删除、大量输入产生问题
            (save-excursion
              (progn
		(when (and *cnhl-sentence-beginning-position*
                           (> *cnhl-sentence-beginning-position* (point)))
                  (goto-char *cnhl-sentence-beginning-position*)
                  (setq *cnhl-sentence-beginning-position* nil))
		(or (search-forward-regexp regxp (line-end-position) t)
                    (line-end-position)))))
           (beg
            ;; 本次编辑的起始点小于光标位置，则从起始点为高亮起点向前搜索
            (progn
              (when (and *cnhl-sentence-beginning-position*
                         (< *cnhl-sentence-beginning-position* (point)))
                (goto-char *cnhl-sentence-beginning-position*)
                (setq *cnhl-sentence-beginning-position* nil))
              (or (search-backward-regexp regxp (line-beginning-position) t)
                  (line-beginning-position)))))
      ;; 对语段进行中文高亮
      (cnhl beg end))))

(defvar *cnhl-sentence-timer* nil
  "用于临时存储cnhl实时高亮的timer")

(defun cnhl-sentence-timely (beg end len)
  "中文实时高亮函数，加入after-change-functions以实现实时高亮输入内容"
  (when (member major-mode *cnhl-major-mode*)
    (if *cnhl-sentence-timer*
        ;; 删除准备执行的高亮
        (cancel-timer *cnhl-sentence-timer*)
      ;; 记录起始点
      (setq *cnhl-sentence-beginning-position* (point)))
    ;; 安排高亮任务，延迟一段时间后执行，防止过于频繁地执行高亮造成资源浪费
    (setq *cnhl-sentence-timer*
          (run-at-time
           ;; 默认延迟0.1秒
           "0.1" nil '(lambda ()
                        (cnhl-sentence)
                        ;; 执行完删除timer
                        (setq *cnhl-sentence-timer* nil))))))

;; cnhl中文分词操作按键绑定
(global-set-key (kbd "M-f") 'cnhl-forward-word)
(global-set-key (kbd "M-b") 'cnhl-backward-word)
(global-set-key (kbd "M-d") 'cnhl-kill-word)
(global-set-key (kbd "M-DEL") 'cnhl-backward-kill-word)

(provide 'cnhl)
;;; cnhl.el ends here
