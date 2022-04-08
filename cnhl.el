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
;;
;; It used THULAC (THU Lexical Analyzer for Chinese) by Tsinghua University.
;; Maosong Sun, Xinxiong Chen, Kaixu Zhang, Zhipeng Guo, Zhiyuan Liu. THULAC: An Efficient Lexical Analyzer for Chinese. 2016.
;;
;; For more infomation, read https://github.com/3vau/cnhl/blob/main/README.md
;; and https://emacs-china.org/t/topic/18977/38
;;
;; Thanks to people who helped me:
;;  @LdBeth http://ldbeth.sdf.org/
;;  @cireu https://citreu.gitlab.io/
;;  @twlz0ne https://emacs-china.org/u/twlz0ne

;;; Code:

(defvar cnhl-install-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Cnhl 的安装目录。请将 cnhl.el, cnhl-thulac.so / cnhl-thulac.dll, cnhl-fasthan.py
放在该文件夹下。")

(defvar cnhl-nlp-selected "fasthan"
  "指定 Cnhl 使用的 NLP 工具类型。
若该值为 \"thulac\" ，则使用 THULAC 工具。请完成 THULAC 模型位置的设置。
若该值为 \"fasthan\" ，则使用 FastHan 工具。请安装 FastHan pip 模组。
默认使用 FastHan 。 ")

(defun cnhl-switch-nlp (nlp)
  (interactive "s请输入你想切换到的 nlp 工具(THULAC / FastHan)：")
  (setq nlp (downcase nlp))
  (pcase nlp
    ("thulac"
     (setq cnhl-nlp-selected "thulac")
     (cnhl-nlp-init t)
     (setq cnhl-nlp-reinit 'reload))
    ("fasthan"
     (setq cnhl-nlp-selected "fasthan")
     (setq cnhl-nlp-reinit 'reload))
    (_ (message "抱歉，未能识别你输入的 nlp 名称。"))))

(defvar cnhl-hl-use-dep-p nil)
(defvar cnhl-word-use-dep-p nil)

(defun cnhl-use-dependency (&optional type)
  "设置 Cnhl 是否使用依存句法分析。

若 type 为 \"hl\" ，则使用依存句法方式进行高亮，使用普通分词方式进行按词操作；
若 type 为 \"word\" ，则使用词法分析方式进行高亮，使用依存句法方式进行按词操作；
若 type 为空或 nil ，则高亮和按词操作都使用词法分析；
若 type 为其它非 nil 的值，则高亮和按词操作都使用依存句法分析。

默认情况下， Cnhl 的高亮和按词操作都使用词法分析。"
  (interactive "s请输入你要切换到的行为模式(\"hl\":句法高亮、词法分词； \"word\":词法高亮、句法分词；空则全词法，其它非nil值则全句法)：")
  (if (and (equal cnhl-nlp-selected "thulac")
	   type)
      (progn (setq cnhl-hl-use-dep-p nil
		   cnhl-word-use-dep-p nil)
	     (message "当前选择的 NLP 不具备依存句法分析能力，自动使用词法分析。"))
    (progn (if type
	       (pcase type
		 ("hl" (setq cnhl-hl-use-dep-p t
			    cnhl-word-use-dep-p nil))
		 ("word" (setq cnhl-hl-use-dep-p nil
			      cnhl-word-use-dep-p t))
		 (_ (setq cnhl-hl-use-dep-p t
			  cnhl-word-use-dep-p t)))
	     (setq cnhl-hl-use-dep-p nil
		   cnhl-word-use-dep-p nil))
	   t))
  (setq cnhl-nlp-reinit t))
;; (cnhl-use-dependency 'hl)

(defun cnhl-thulac-compile-module ()
  (if (file-exists-p (expand-file-name "cnhl-thulac.cpp"
				       cnhl-install-dir))
      (async-shell-command
       (format "git clone --depth 1 https://github.com/thunlp/THULAC.git %s ; g++ -shared -I %s -std=c++11 %s -o cnhl-thulac.dll"
	       (expand-file-name "thulac/"
				 cnhl-install-dir)
	       (expand-file-name "thulac/include/"
				 cnhl-install-dir)
	       (expand-file-name "cnhl-thulac.cpp"
				 cnhl-install-dir)
	       (expand-file-name (if (equal system-type 'windows-nt)
				     "cnhl-thulac.dll"
				   "cnhl-thulac.so"))))
    (error "cnhl-thulac.cpp 源文件不存在，请重新下载 Cnhl。")))

(defvar cnhl-thulac-module-path
  cnhl-install-dir
  "THULAC 算法模型文件夹的目录。")

(defvar cnhl-fasthan-use-large-model-p nil
  "是否使用 FastHan 的 large 模型。

Base 模型占用 350M 左右的内存； large 模型占用 450M 左右的内存。

默认使用 base 模型。

若想实时生效，请在设置后手动执行 \"(cnhl-nlp-init t)\"。")

(defvar cnhl-nlp-initialized nil
  "指示 Cnhl 是否已经初始化。")

(defvar cnhl-nlp-reinit nil
  "指示是否需要在下次使用 cnhl 函数时重新进行 nlp 初始化设置。
若该值为 'reload ，则在下次使用时重新加载 nlp 模型并绑定函数；
若该值为其它非 nil 的值，则在下次使用时只重新绑定函数，不重新加载 nlp 模型。")

(defun cnhl-nlp-init (&optional restart-nlp-p)
  (setq restart-nlp-p (or restart-nlp-p
			  (null cnhl-nlp-initialized)))
  (when restart-nlp-p
    (when cnhl-fasthan-epc
      (cnhl-nlp-deinit-fasthan))
    (cnhl-nlp-deinit-thulac))
  (advice-remove 'cnhl-nlp-analyse-sentence 'analyse-func)
  (advice-remove 'cnhl-nlp-get-overlay 'overlay-func)
  (pcase cnhl-nlp-selected
    ("thulac"
     (when restart-nlp-p (cnhl-nlp-init-thulac))
     (advice-add 'cnhl-nlp-analyse-sentence
		 :override #'cnhl-thulac-analyse-sentence
		 (list (cons 'name 'analyse-func)))
     (advice-add 'cnhl-nlp-get-overlay
		 :override #'cnhl-get-overlay-thulac
		 (list (cons 'name 'analyse-func))))
    ("fasthan"
     (when restart-nlp-p (cnhl-nlp-init-fasthan))
     (advice-add 'cnhl-nlp-analyse-sentence
		 :override #'cnhl-fasthan-analyze-sentence
		 (list (cons 'name 'analyse-func)))
     (if cnhl-hl-use-dep-p
	 (advice-add 'cnhl-nlp-get-overlay
		     :override #'cnhl-get-overlay-dep
		     (list (cons 'name 'analyse-func)))
       (advice-add 'cnhl-nlp-get-overlay
		   :override #'cnhl-get-overlay-stanford
		   (list (cons 'name 'analyse-func))))))
  (setq cnhl-nlp-initialized t
	cnhl-nlp-reinit nil))

(defun cnhl-nlp-reinit-check ()
  (when cnhl-nlp-reinit
    (pcase cnhl-nlp-reinit
      ('reload (cnhl-nlp-init t))
      (_ (cnhl-nlp-init)))))

(defun cnhl-nlp-init-thulac ()
  (condition-case err
      (require 'cnhl-thulac)
    ('file-missing (cnhl-thulac-compile-module)
		   (require 'cnhl-thulac)))
  (cnhl-thulac-module-init cnhl-thulac-module-path))

(defun cnhl-nlp-deinit-thulac ()
  (condition-case err
      (cnhl-thulac-module-deinit)
    (t nil)))

(defvar cnhl-fasthan-epc nil)

(defun cnhl-nlp-init-fasthan ()
  (require 'epc)
  (setq cnhl-fasthan-epc
	(epc:start-epc "python"
		       `(,(expand-file-name "cnhl-fasthan.py"
					    cnhl-install-dir))))
  (epc:call-sync cnhl-fasthan-epc 'fasthan_init_model
		 `(,@(if cnhl-fasthan-use-large-model-p
			 (list "large" "")
		       (list "base" "")))))

(defun cnhl-nlp-deinit-fasthan ()
  (epc:stop-epc cnhl-fasthan-epc)
  (setq cnhl-fasthan-epc nil))

(defgroup cnhl nil
  "Cnhl 高亮颜色。"
  :group 'cnhl)
(defface cnhl-face-1
  '((t (:foreground "#FFCCCC")))
  "第一种，在 THULAC 中是名词、代词、简称颜色"
  :group 'cnhl)
(defface cnhl-face-2
  '((t (:foreground "#BFEBE0")))
  "第二种，在 THULAC 中是动词、习语颜色"
  :group 'cnhl)
(defface cnhl-face-3
  '((t (:foreground "#C6EAFF")))
  "第三种，在 THULAC 中是形容词颜色"
  :group 'cnhl)
(defface cnhl-face-4
  '((t (:foreground "#F8DEC0")))
  "第四种，在 THULAC 中是方位词、处所词、时间词、数词、量词、数量词颜色"
  :group 'cnhl)
(defface cnhl-face-5
  '((t (:foreground "#DFDFB0")))
  "第五种，在 THULAC 中是副词、连词、介词颜色"
  :group 'cnhl)
(defface cnhl-face-6
  '((t (:foreground "#E5CFEF")))
  "第六种，在 THULAC 中是助词、语气助词、前接成分、后接成分颜色"
  :group 'cnhl)
(defface cnhl-face-7
  '((t (:foreground "gray85")))
  "第七种，在 THULAC 中是语素、标点、叹词、拟声词及其它颜色"
  :group 'cnhl)

;; dark

;; (set-face-foreground 'cnhl-face-1 "#5F0000")
;; (set-face-foreground 'cnhl-face-2 "#184034")
;; (set-face-foreground 'cnhl-face-3 "#093060")
;; (set-face-foreground 'cnhl-face-4 "#5D3026")
;; (set-face-foreground 'cnhl-face-5 "#3F3000")
;; (set-face-foreground 'cnhl-face-6 "#541F4F")
;; (set-face-foreground 'cnhl-face-7 "gray15")

(defvar cnhl-overlay-1 (make-overlay 1 1))
(defvar cnhl-overlay-2 (make-overlay 1 1))
(defvar cnhl-overlay-3 (make-overlay 1 1))
(defvar cnhl-overlay-4 (make-overlay 1 1))
(defvar cnhl-overlay-5 (make-overlay 1 1))
(defvar cnhl-overlay-6 (make-overlay 1 1))
(defvar cnhl-overlay-7 (make-overlay 1 1))

(overlay-put cnhl-overlay-1 'face 'cnhl-face-1)
(overlay-put cnhl-overlay-2 'face 'cnhl-face-2)
(overlay-put cnhl-overlay-3 'face 'cnhl-face-3)
(overlay-put cnhl-overlay-4 'face 'cnhl-face-4)
(overlay-put cnhl-overlay-5 'face 'cnhl-face-5)
(overlay-put cnhl-overlay-6 'face 'cnhl-face-6)
(overlay-put cnhl-overlay-7 'face 'cnhl-face-7)

(defun cnhl-nlp-get-overlay (str)
  (cnhl-nlp-init)
  (cnhl-nlp-get-overlay str))

(defvar cnhl-overlay-alist-thulac
  (list (cons "n" cnhl-overlay-1)
	(cons "r" cnhl-overlay-1)
	(cons "j" cnhl-overlay-1)
	(cons "u" cnhl-overlay-6)
	(cons "y" cnhl-overlay-6)
	(cons "h" cnhl-overlay-6)
	(cons "k" cnhl-overlay-6)
	(cons "v" cnhl-overlay-2)
	(cons "i" cnhl-overlay-2)
	(cons "a" cnhl-overlay-3)
	(cons "d" cnhl-overlay-5)
	(cons "c" cnhl-overlay-5)
	(cons "p" cnhl-overlay-5)
	(cons "g" cnhl-overlay-7)
	(cons "w" cnhl-overlay-7)
	(cons "x" cnhl-overlay-7)
	(cons "e" cnhl-overlay-7)
	(cons "o" cnhl-overlay-7))
  "存储词性标记首字母与 overlay 对应关系的 alist")

(defun cnhl-get-overlay-thulac (str)
  "匹配词性类型对应的face"
  (or (cdr (assoc (string (aref str 0)) cnhl-overlay-alist-thulac))
      cnhl-overlay-4)) ;; 用首字母从 alist 中获取值

(defvar cnhl-overlay-alist-stanford
  (list (cons "VA" cnhl-overlay-3)
	(cons "VC" cnhl-overlay-2)
	(cons "VE" cnhl-overlay-2)
	(cons "VV" cnhl-overlay-2)
	(cons "NR" cnhl-overlay-1)
	(cons "NT" cnhl-overlay-1)
	(cons "NN" cnhl-overlay-1)
	(cons "LC" cnhl-overlay-4)
	(cons "PN" cnhl-overlay-1)
	(cons "DT" cnhl-overlay-4)
	(cons "CD" cnhl-overlay-4)
	(cons "OD" cnhl-overlay-4)
	(cons "M" cnhl-overlay-4)
	(cons "AD" cnhl-overlay-5) 
	(cons "P" cnhl-overlay-5)
	(cons "CC" cnhl-overlay-5)
	(cons "CS" cnhl-overlay-5)
	(cons "DEC" cnhl-overlay-5)
	(cons "DEG" cnhl-overlay-5)
	(cons "DER" cnhl-overlay-5)
	(cons "DEV" cnhl-overlay-5)
	(cons "AS" cnhl-overlay-6)
	(cons "SP" cnhl-overlay-6)
	(cons "ETC" cnhl-overlay-6)
	(cons "MSP" cnhl-overlay-6)
	(cons "IJ" cnhl-overlay-7)
	(cons "ON" cnhl-overlay-7)
	(cons "LB" cnhl-overlay-5)
	(cons "SB" cnhl-overlay-5)
	(cons "BA" cnhl-overlay-5)
	(cons "JJ" cnhl-overlay-3)
	(cons "FW" cnhl-overlay-1)
	(cons "PU" cnhl-overlay-7)))

(defun cnhl-get-overlay-stanford (str)
  (or (cdr (assoc str cnhl-overlay-alist-stanford))
      cnhl-overlay-7))

(defvar cnhl-overlay-alist-dep
  (list (cons "ubj" cnhl-overlay-1)
	(cons "ass" cnhl-overlay-1)
	(cons "obj" cnhl-overlay-3)
	(cons "oot" cnhl-overlay-6)
	(cons "ssm" cnhl-overlay-5)
	(cons "omp" cnhl-overlay-2)
	(cons "onj" cnhl-overlay-4)
	(cons "nct" cnhl-overlay-7)))

(defun cnhl-get-overlay-dep (tag)
  (assoc-default tag cnhl-overlay-alist-dep))

(defun cnhl-generate-hl-word ()
  (cl-loop for tag in cnhl-last-prop-list
	   for word in cnhl-last-word-list
	   collect (make-list (length word) tag) into r
	   finally (cl-return (flatten-list r))))

(defun cnhl-generate-hl-dep (&optional giving-result)
  (let ((holding 0)
	(r))
    (cl-loop for i = 0 then (1+ i)
	     with dep-list = (or (cadr giving-result) cnhl-last-dep-list)
	     and word-list = (or (car giving-result) cnhl-last-word-list)
	     for tag in dep-list
	     if (cnhl-dep-check-independent tag)
	     do (progn
		  (dotimes (_ (+ holding
				 (length (nth i word-list))))
		    (let ((l (length tag)))
		      (push (substring tag (- l 3) l) r)))
		  (setq holding 0))
	     else do (setq holding
			   (+ holding
			      (length (nth i word-list))))
	     finally (when (not (= holding 0))
		       (dotimes (_ holding)
			   (push "nct" r))))
    (reverse r)))

(defvar cnhl-sentence-max-length 100)

(defvar cnhl-punc-regexp-word
  "[，。？；：、‘’“”…—！（）～《》「」【】〖〗『』〔〕,.?!():;/\\*#]")

(defvar cnhl-punc-regexp-dep
  "[。？；：…—！（）～《》「」【】〖〗『』〔〕.?!():;/\\*#]")

(defun cnhl-detect-sentence (&optional beg end)
  (save-excursion
    (unless end
      (unless beg
	(setq beg (point)))
      (setq end beg))
    (let* ((max-len (/ cnhl-sentence-max-length 2))
	   (min-pos (max (- beg max-len) (point-min)))
	   (max-pos (min (+ end max-len) (point-max)))
	   (regexp (if cnhl-hl-use-dep-p
		       cnhl-punc-regexp-dep
		     cnhl-punc-regexp-word))
	   (beg-r (or (progn
			(goto-char beg)
			(search-backward-regexp regexp min-pos t))
		      min-pos))
	   (end-r (or (progn
			(goto-char end)
			(search-forward-regexp regexp max-pos t))
		      max-pos)))
      (list beg-r end-r))))

(defvar cnhl-content-regexp
  "[\u2e80-\u9fa5，。？；：、‘’“”…—！（）～《》「」【】〖〗『』〔〕,.?!():;/\\*#a-zA-Z0-9]")

(defvar cnhl-not-content-regexp
  "[^\u2e80-\u9fa5，。？；：、‘’“”…—！（）～《》「」【】〖〗『』〔〕,.?!():;/\\*#a-zA-Z0-9]")

(defun cnhl-string-pretreatment (beg end)
  (replace-regexp-in-string cnhl-not-content-regexp ""
			    (buffer-substring-no-properties beg end)))

;; test: (apply #'cnhl-string-pretreatment (cnhl-detect-sentence 24033))

(defvar cnhl-last-word-list nil
  "词语列表，存储分词后的所有词汇们。")
(defvar cnhl-last-prop-list nil
  "词性列表，存储与被分析句的字数相对应数量的词性标记
使用何种词性标记由 NLP 决定。")
(defvar cnhl-last-region-list (list 0 0)
  "上次分析的句子的起始与结束位置。")
(defvar cnhl-last-dep-list nil
  "依存关系列表，存储依存句法分析后的每个词的依存性质。")
(defvar cnhl-last-targ-list nil
  "存储每个词的依存关系所指向的词在句中的位置。")

(defun cnhl-nlp-analyse-sentence (&optional beg end)
  (unless cnhl-nlp-initialized
    (cnhl-nlp-init)
    (cnhl-nlp-analyse-sentence beg end)))

(defun cnhl-thulac-string-process (str)
  (setq str (string-trim
	     (replace-regexp-in-string
	      "\n" "" str))
	str (replace-regexp-in-string
	     "\s\s_w" "" str))
  (let ((word-prop-lst (split-string str " "))
	(word-lst nil)
	(prop-lst nil))
    (dolist (item word-prop-lst)
      (let* ((pos (string-match "_[a-z]+$" item))
	     (word (substring item 0 pos))
	     (prop (substring item (1+ pos))))
	(push word word-lst) ;; 插入词语
	(push prop prop-lst)))
    (cons (reverse word-lst) (reverse prop-lst))))

(defun cnhl-thulac-analyse-sentence (&optional beg end)
  (cnhl-nlp-reinit-check)
  (unless (and (bound-and-true-p end)
	       (>= beg end))
    (let* ((region (cnhl-detect-sentence beg end))
	   (result (cnhl-thulac-string-process
		    (cnhl-thulac-string
		     (apply #'buffer-substring-no-properties region)))))
      (setq cnhl-last-word-list (car result)
	    cnhl-last-prop-list (cdr result)
	    cnhl-last-region-list region))))

;; (cnhl-nlp-init)
;; (cnhl-nlp-analyse-sentence 25141)

(defun cnhl-fasthan-analyze-sentence (&optional beg-or-sentence end giving-result)
  (cnhl-nlp-reinit-check)
  (cl-loop with sentencep = (and (bound-and-true-p beg-or-sentence)
				 (stringp beg-or-sentence))
	   with region = (if giving-result
			     (list beg-or-sentence end)
			   (unless sentencep
			     (let ((r (cnhl-detect-sentence beg-or-sentence end)))
			       (setq beg-or-sentence
				     (apply #'buffer-substring-no-properties r))
			       r)))
	   and result = (or giving-result
			    (car (epc:call-sync
				  cnhl-fasthan-epc
				  'fasthan_parsing_string
				  (list beg-or-sentence))))
	   for r in result
	   unless (string-match-p "\\`[\s\n]+\\'" (car r))
	   collect (replace-regexp-in-string "[\s\n\u3000]" "" (car r)) into word-list
	   and collect (cadr r) into targ-list
	   and collect (caddr r) into dep-list
	   and collect (cadddr r) into prop-list
	   finally do (setq cnhl-last-word-list word-list
			    cnhl-last-dep-list dep-list
			    cnhl-last-targ-list targ-list
			    cnhl-last-prop-list prop-list
			    cnhl-last-region-list region)
	   finally return (list word-list dep-list targ-list prop-list region)))

;; cnhl-last-word-list
;; cnhl-last-region-list
;; (mapcar #'(lambda (i) (insert (concat i " "))) cnhl-last-dep-list)

;; (string-match-p  "\\`[\s\n]+\\'" "       \n\n  ")
;; (cnhl-fasthan-analyze-sentence 39426 39486)

(defvar cnhl-dep-independent-tag-end
  (list "ubj" "obj" "oot" "ssm" "omp" "onj" "nct" "ass"))
(defvar cnhl-dep-end-list nil)

(defun cnhl-dep-generate-end-list ()
  (setq cnhl-dep-end-list
	(list (mapcar #'(lambda (i) (aref i 0))
		      cnhl-dep-independent-tag-end)
	      (mapcar #'(lambda (i) (aref i 1))
		      cnhl-dep-independent-tag-end)
	      (mapcar #'(lambda (i) (aref i 2))
		      cnhl-dep-independent-tag-end))))
(cnhl-dep-generate-end-list)

(defun cnhl-dep-check-independent (tag)
  (condition-case nil
      (let ((l (length tag)))
	(when (and (member (aref tag (- l 3)) (car cnhl-dep-end-list))
		   (member (aref tag (- l 2)) (cadr cnhl-dep-end-list))
		   (member (aref tag (- l 1)) (caddr cnhl-dep-end-list))
		   (not (member tag (list "pass" "auxpass" "asp"))))
	  t))
    (t nil)))
;; (cnhl-dep-check-independent "asp")

;; (save-excursion
;;   (profiler-start 'cpu+mem)
;;   (goto-char 16056)
;;   (dotimes (i 600)
;;     (face-at-point)

;;     (forward-char))
;;   (profiler-stop)
;;   (profiler-report))

(defun cnhl-hl (&optional giving-result)
  (save-excursion
    (goto-char (car (or (car (last giving-result))
			cnhl-last-region-list)))
    (let ((lst (if cnhl-hl-use-dep-p
		   (cnhl-generate-hl-dep giving-result)
		 (cnhl-generate-hl-word))))
      (while lst
	(when (string-match-p "[^\s\n\u3000]"
			      (char-to-string (following-char)))
	  (if (let ((f (face-at-point)))
		(or (null f)
		    (string= (substring (symbol-name f) 0 4)
			     "cnhl")))
	      (move-overlay
	       (copy-overlay (cnhl-nlp-get-overlay (pop lst)))
	       (point) (1+ (point))
	       (current-buffer))
	    (pop lst)))
	(forward-char)))))
;; (cnhl-nlp-analyse-sentence 26763)
;; (cnhl-hl)

(defvar cnhl-after-change-timer nil)
(defvar cnhl-after-change-begin nil)
(defvar cnhl-after-change-waiting "0.5")

(defun cnhl-hl-after-change (beg end len)
  (when cnhl-mode
    (if cnhl-after-change-timer
	(cancel-timer cnhl-after-change-timer)
      (setq cnhl-after-change-beginning beg))
    (setq cnhl-after-change-timer
	  (run-at-time
	   cnhl-after-change-waiting
	   nil
	   #'(lambda ()
	       (setq cnhl-after-change-timer nil
		     cnhl-last-region-list (cnhl-detect-sentence
					    cnhl-after-change-beginning
					    (point)))
	       (if cnhl-hl-use-dep-p
		   (deferred:$
		    (epc:call-deferred
		     cnhl-fasthan-epc
		     'fasthan_parsing_string
		     (list (apply #'buffer-substring-no-properties
				  cnhl-last-region-list)))
		    (deferred:nextc it (lambda (x)
					 (apply #'cnhl-nlp-analyse-sentence
						(append cnhl-last-region-list
							x))))
		    (deferred:nextc it (lambda (x) (cnhl-hl x))))
		 (progn (cnhl-nlp-analyse-sentence)
			(cnhl-hl))))))))

(defun cnhl-hl-buffer ()
  " 一口气高亮整个 buffer 。注意，若使用依存句法分析进行高亮将会较慢。"
  (interactive)
  (cnhl-nlp-analyse-sentence (point-min) (- (point-max) 2))
  (cnhl-hl))

(defun cnhl-hl-paragraph ()
  "高亮光标所在段落。"
  (interactive)
  (save-excursion
    (cnhl-nlp-analyse-sentence
     (progn (backward-paragraph)
	    (search-forward-regexp "[^\s]"))
     (progn (forward-paragraph)
	    (1- (search-backward-regexp "[^\s]")))))
  (cnhl-hl))

(defun cnhl-hl-sentence ()
  "高亮光标所在句。"
  (interactive)
  (cnhl-nlp-analyse-sentence)
  (cnhl-hl))

(defvar cnhl-get-word-time nil)

(defun cnhl-get-word-pos-arround ()
  (let ((beg (car cnhl-last-region-list))
	(end (cadr cnhl-last-region-list))
	(p-now (point)))
    (if (and (or (>= p-now end)
		 (<= p-now beg))
	     (null cnhl-get-word-time))
	(progn (cnhl-nlp-analyse-sentence
		p-now (1+ p-now))
	       (setq cnhl-get-word-time t)
	       (cnhl-get-word-pos-arround))
      (save-excursion
	(when cnhl-get-word-time
	  (setq cnhl-get-word-time nil))
	(goto-char beg)
	(if cnhl-word-use-dep-p
	    (cl-loop for word in cnhl-last-word-list
		     for p = beg then (search-forward word)
		     for tag in cnhl-last-dep-list
		     with prev-tag
		     when (and (cnhl-dep-check-independent tag) ;; indent 0
			       (not (equal tag prev-tag)))
		     do (setq prev-tag tag) ;; indent 1
		     and collect p into prev-p ;; indent 1
		     and if (> p p-now) ;; indent 1
		     if (< (length prev-p) 3) ;; indent 2
		     do (progn (cnhl-nlp-analyse-sentence ;; indent 3
				(1- beg) beg)
			       (cnhl-get-word-pos-arround))
		     else return (last prev-p 3) ;; indent 2
		     else do '(nil)) ;; indent 1
	  (cl-loop for word in cnhl-last-word-list
		   for p = beg then (search-forward word)
		   collect p into prev-p
		   until (> p p-now)
		   finally return (last prev-p 3)))))))

(define-advice forward-word
    (:around (orig-func &optional arg)
	     cnhl-forward-word)
  (if cnhl-mode
      (condition-case err
	  (let ((p (point)))
	    (if (< arg 0)
		(dotimes (i (- arg))
		  (goto-char (car (cnhl-get-word-pos-arround))))
	      (dotimes (i (or arg 1))
		(goto-char (caddr (cnhl-get-word-pos-arround)))))
	    t)
	(t nil))
    (funcall orig-func arg)))

(load "simple.el")
(load "subr.el")

(defvar cnhl-dep-meaning-alist
  (list (cons "root" "根谓语")
	(cons "punct" "标点")
	(cons "subj" "主语")
	(cons "nsubj" "主语")
	(cons "nsubjpass" "主语(被动)")
	(cons "top" "主题")
	(cons "npsubj" "主语(被动)")
	(cons "csubj" "主语(子句)")
	(cons "xsubj" "主语(子句)")
	(cons "obj" "宾语")
	(cons "dobj" "宾语(直接)")
	(cons "iobj" "宾语(间接)")
	(cons "range" "宾语(间接，数量词)")
	(cons "pobj" "宾语(介词)")
	(cons "lobj" "时间介词")
	(cons "comp" "补语")
	(cons "ccomp" "补语(子句)")
	(cons "xcomp" "补语(子句)")
	(cons "acomp" "补语(形容词)")
	(cons "tcomp" "补语(时间)")
	(cons "lccomp" "补语(位置)")
	(cons "rcomp" "补语(结果)")
	(cons "asp" "助词")
	(cons "mod" "修饰")
	(cons "pass" "修饰(被动)")
	(cons "tmod" "修饰(时间)")
	(cons "rcmod" "修饰(关系子句)")
	(cons "numod" "修饰(数量)")
	(cons "ornmod" "修饰(序数)")
	(cons "clf" "修饰(类别)")
	(cons "nmod" "修饰(复合名词)")
	(cons "amod" "修饰(形容词)")
	(cons "advmod" "修饰(副词)")
	(cons "vmod" "修饰(动词)")
	(cons "prnmod" "修饰(插入词)")
	(cons "neg" "修饰(不定)")
	(cons "det" "修饰(限定)")
	(cons "nn" "修饰(名词)")
	(cons "nummod" "修饰(数词)")
	(cons "possm" "所属标记")
	(cons "poss" "修饰(所属)")
	(cons "dvpm" "状中标记")
	(cons "dvpmod" "修饰(状中)")
	(cons "assm" "关联标记")
	(cons "assmod" "修饰(关联)")
	(cons "prep" "修饰(介词)")
	(cons "clmod" "修饰(子句)")
	(cons "plmod" "修饰(介词，地点)")
	(cons "csp" "时态标词")
	(cons "partmod" "修饰(分词)")
	(cons "etc" "等")
	(cons "conj" "并列词")
	(cons "cop" "系动") ;; *
	(cons "cc" "并列连接词")
	(cons "attr" "定语")
	(cons "cordmod" "并列联合词")
	(cons "mmod" "情态动词")
	(cons "ba" "把字句标词")
	(cons "tclaus" "时间子句")
	(cons "cpm" "补语化成分")
	(cons "auxpass" "被动词")
	(cons "case" "依赖关系") ;; *
	(cons "relcl" "依赖关系")
	(cons "nfincl" "依赖关系")))

(defvar cnhl-prop-meaning-alist-stanford
  (list (cons "VA" "形容词(谓词性)")
	(cons "VC" "系动词")
	(cons "VE" "动词(存在性)")
	(cons "VV" "动词")
	(cons "NR" "专有名词")
	(cons "NT" "名词(时间)")
	(cons "NN" "名词")
	(cons "LC" "方位词")
	(cons "PN" "代词")
	(cons "DT" "限定词")
	(cons "CD" "基数词")
	(cons "OD" "序数词")
	(cons "M" "度量词")
	(cons "AD" "副词")
	(cons "P" "介词")
	(cons "CC" "并列连接词")
	(cons "CS" "从属连接词")
	(cons "DEC" "的(补语)") ;; *
	(cons "DEG" "的(偏正)")
	(cons "DER" "得(补语)")
	(cons "DEV" "地(偏正)")
	(cons "AS" "助动词") 
	(cons "SP" "助词(句末)") ;; *
	(cons "ETC" "等") ;; *
	(cons "MSP" "助词")
	(cons "IJ" "感叹词")
	(cons "ON" "拟声词")
	(cons "LB" "被") ;; *
	(cons "SB" "被") ;; *
	(cons "BA" "把")
	(cons "JJ" "修饰词")
	(cons "FW" "外来词") ;; *
	(cons "PU" "标点")))

(defun cnhl-analyze-sentence (&optional sentence)
  (interactive)
  (when sentence
    (cnhl-fasthan-analyze-sentence sentence))
  (save-excursion (insert "\n\n\n\n\n\n"))
  (next-line)
  (cl-loop for i from 1 to (length cnhl-last-targ-list)
	   for word in cnhl-last-word-list
	   for targ in cnhl-last-targ-list
	   for dep in cnhl-last-dep-list
	   for prop in cnhl-last-prop-list
	   for i-str = (number-to-string i)
	   for dep-with-meaning = (concat
				   (assoc-default dep cnhl-dep-meaning-alist)
				   "(" dep ")")
	   for targ-with-meaning = (concat
				   (number-to-string targ)
				   "(" (nth (- targ 1) cnhl-last-word-list) ")")
	   for prop-with-meaning = (concat
				    (assoc-default prop
						   cnhl-prop-meaning-alist-stanford)
				    "(" prop ")")
	   for lengthes = (mapcar
			   #'(lambda (i)
			       (length
				(replace-regexp-in-string
				 "[\u2000-\u206f\u3000-\u9fff\uff00-\uffef]"
				 "aa" i)))
			   (list word
				 targ-with-meaning
				 dep-with-meaning
				 prop-with-meaning))
	   for distance = (+ 1 (apply #'max lengthes))
	   for total-distance = distance then (+ distance total-distance)
	   when (> total-distance (window-width))
	   do (progn (forward-line 5)
		     (save-excursion (insert "\n\n\n\n\n\n"))
		     (next-line)
		     (setq total-distance distance))
	   do (save-excursion
		(goto-char (line-end-position))
		(insert i-str (make-string (- distance (length i-str)) 32))
		(next-line)
		(goto-char (line-end-position))
		(insert word (make-string (- distance (car lengthes)) 32))
		(next-line)
		(goto-char (line-end-position))
		(insert targ-with-meaning
			(make-string (- distance (cadr lengthes)) 32))
		(next-line)
		(goto-char (line-end-position))
		(insert dep-with-meaning
			(make-string (- distance (caddr lengthes)) 32))
		(next-line)
		(goto-char (line-end-position))
		(insert prop-with-meaning
			(make-string (- distance (cadddr lengthes)) 32)))))

(defcustom cnhl-lighter
  " Cnhl"
  "Cnhl 的 Mode line 提示符。"
  :type '(choice (const :tag "No lighter" "") string)
  :safe 'stringp)

(defcustom cnhl-mode-hook '()
  "flex mode hook."
  :type 'hook
  :group 'cnhl)

(define-minor-mode cnhl-mode
  "Cnhl mode."
  :init-value nil
  :lighter cnhl-lighter
  (cnhl-nlp-init)
  (add-hook 'after-change-functions 'cnhl-hl-after-change)
  (unless (advice-member-p 'forward-word@cnhl-forward-word
			   'forward-word)
    (define-advice forward-word
        (:around (orig-func &optional arg)
    	     cnhl-forward-word)
      (if cnhl-mode
          (condition-case err
    	  (let ((p (point)))
    	    (if (< arg 0)
    		(dotimes (i (- arg))
    		  (goto-char (car (cnhl-get-word-pos-arround))))
    	      (dotimes (i (or arg 1))
    		(goto-char (caddr (cnhl-get-word-pos-arround)))))
    	    t)
    	(t nil))
        (funcall orig-func arg)))
    
    (load "simple.el")
    (load "subr.el")
    )
  (run-hooks 'cnhl-mode-hook))

(provide 'cnhl)

;;; cnhl.el ends here
