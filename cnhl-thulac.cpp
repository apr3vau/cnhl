#include <iostream>
#include <emacs-module.h>
#include "thulac/include/thulac.h"

using namespace std;

int plugin_is_GPL_compatible;

THULAC t;

bool initialized = false;

static char *
retrieve_string (emacs_env *env, emacs_value str)
{
  char *buf = NULL;
  ptrdiff_t size = 0;

  env->copy_string_contents (env, str, NULL, &size);

  buf = (char *) malloc (size);
  if (buf == NULL) return NULL;

  env->copy_string_contents (env, str, buf, &size);

  return buf;
}

static emacs_value
Fcnhl_thulac_module_init(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) noexcept
{
  if (initialized)
    {
      t.deinit();
    }
  string module_path = retrieve_string(env, args[0]);
  t.init(module_path.data(), NULL, 0, 0, 0, '_');
  cout << "THULAC initialized!" << endl;
  initialized = true;
  return env->intern(env, "t");
}

static emacs_value
Fcnhl_thulac_module_deinit(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) noexcept
{
  if (initialized)
    {
      t.deinit();
    }
  initialized = false;
  return env->intern(env, "t");
}

static emacs_value
Fcnhl_thulac_string(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) noexcept
{
  if (initialized != true)
    {
      cout << "THULAC module hasn't initialized!" << endl;
      return env->intern(env, "");
    }
  string s = retrieve_string(env, args[0]);
  THULAC_result r = t.cut(s);
  s = t.toString(r);
  return env->make_string(env, s.data(), s.length());
}

static void
provide (emacs_env *env, const char *feature)
{
    emacs_value Qfeat = env->intern (env, feature);
    emacs_value Qprovide = env->intern (env, "provide");
    emacs_value args[] = { Qfeat };

    env->funcall (env, Qprovide, 1, args);
}

static void
bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
    emacs_value Qfset = env->intern (env, "fset");
    emacs_value Qsym = env->intern (env, name);
    emacs_value args[] = { Qsym, Sfun };

    env->funcall (env, Qfset, 2, args);
}

int
emacs_module_init(struct emacs_runtime *ert) noexcept
{

  emacs_env *env = ert->get_environment (ert);

#define DEFUN(lsym, csym, amin, amax, doc, data)			\
  bind_function (env, lsym,						\
		 env->make_function (env, amin, amax, csym, doc, data))
  DEFUN("cnhl-thulac-string", Fcnhl_thulac_string, 1, 1, "Send string to THULAC and return the result.", NULL);
  DEFUN("cnhl-thulac-module-init", Fcnhl_thulac_module_init, 1, 1, "Load THULAC module.", NULL);
  DEFUN("cnhl-thulac-module-deinit", Fcnhl_thulac_module_deinit, 0, 0, "Deinit THULAC module.", NULL);

#undef DEFUN

  provide(env, "cnhl-thulac");
  return 0;
}
