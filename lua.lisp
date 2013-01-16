(ql:quickload "cl-lex")
(load "cl-yacc/yacc.fasl")
(defpackage :cl-lua
  (:use cl-lex yacc cl)
  (:export parse-lua lua-lexer))
(in-package :cl-lua)
(defun parse-funcall-standard (fun args)
  `(apply ,fun ,args))
(defun mid-val (a b c)
  b)

(defun nil-fun (a b)
  ())

(defun infix->prefix (a b c)
  (list b c a))

(defun add-list (args)
  `(list ,@args))

(defun make-singelton-list (arg)
  `(list ,arg))

(defun unpack-explist (exp comma explist)
  `(,exp ,@explist))
(cl-lex:define-string-lexer lua-lexer
			    ("\"(\\\\\"|\\\\|[^\"\\\\])*\"" (return (values 'string (read-from-string $@)))) ;yes I am lazy. Replace read-from-string later. I swear this regex will match strings. TODO: single-quoted strings
			    ("(function|do|end|while|repeat|until|if|then|elseif|else|local|return|break)\\b" (return (values (intern (string-upcase $1)) (intern (string-upcase $1)))))
			    ("\\(" (return (values 'left-paren 'left-paren)))
			    ("\\)" (return (values 'right-paren 'right-paren)))
			    ("\\{" (return (values 'left-curly 'left-curly)))
			    ("\\}" (return (values 'right-curly 'right-curly)))
			    ("\\[" (return (values 'left-square 'left-square)))
			    ("\\]" (return (values 'right-square 'right-square)))
			    ("\\:" (return (values 'colon 'colon)))
			    ("\\;" (return (values 'semicolon 'semicolon)))
			    ("\\," (return (values 'comma 'comma)))
			    ("\\." (return (values 'dot 'dot)))
			    ("\\.\\." (return (values 'twodots 'twodots)))
			    ("\\.\\.\\." (return (values 'threedots 'threedots)))
			    ("\\#" (return (values 'hashtag 'hashtag)))
			    ("\\+|\\-|\\*|\\/|\\^|\\%|\\<|\\<\\=|\\>|\\>\\=|\\=\\=|\\~\\=|and|or|not|\\-" (return (values (intern (string-capitalize $@)) (intern (string-capitalize $@)))))
			    ("nil" (return (values 'lua-nil nil)))
			    ("false" (return (values 'false nil)))
			    ("true" (return (values 'true t)))
			    
			    ("[0-9][0-9]*(\\.[0-9][0-9]*)?" (return (values 'number (read-from-string $@)))) ;read-from-string not the best.
			    ("[A-Za-z_][A-Za-z_0-9]*" (return (values 'name (intern $@)))));identifiers

(yacc:define-parser *lua-parser*
		    (:start-symbol chunk)
		    (:terminals ('string function do end while repeat until if then elseif else local 
					 return break left-paren right-paren left-curly right-curly left-square right-square colon comma semicolon threedots + -
					 * / ^ % twodots < <= > >= == ~= and or not - lua-nil false true name number string))
		    (chunk 
		     stats)
		    (block chunk)
		    (stats
		     stat
		     (stat stats)
		     (stat semicolon)
		     (stat semicolon stats)
		     laststat
		     (laststat semicolon))
		    (stat
		     (varlist = explist)
		     functioncall
		     (do block end)
		     (while exp do block end)
		     (repeat block until exp)
		     ifstat
		     (for name = exp comma exp do block end)
		     (for name = exp comma exp comma exp do block end)
		     (for namelist in explist do block end)
		     (function funcname funcbody)
		     (local function name funcbody)
		     (local namelist)
		     (local namelist = explist))
		    (laststat
		     return
		     break
		     (return explist))
		    (funcname
		     nestname
		     (nestname colon name))
		    (nestname
		     name
		     (name dot nestname))
		    (varlist
		     var
		     (var comma varlist))
		    (var
		     name
		     (prefixexp left-square exp right-square)
		     (prefixexp dot name))
		    (namelist
		     name
		     (name comma namelist))
		    (explist*
		     (exp)
		     (exp comma explist* #'unpack-explist))
		    (explist
		     (explist* #'add-list))
		    (exp
		     lua-nil
		     false
		     true
		     number
		     string
		     threedots
		     fundef
		     prefixexp
		     tableconstructor
		     (exp binop exp #'infix->prefix)
		     unop exp)
		    (prefixexp
		     var
		     functioncall
		     (left-paren exp right-paren))
		    (functioncall
		     (prefixexp args #'parse-funcall-standard)
		     (prefixexp colon name args))
		    (args
		     (left-paren right-paren #'nil)
		     (left-paren explist right-paren #'mid-val)
		     tableconstructor
		     (string #'make-singelton-list))
		    (fundef
		     (function funcbody))
		    (funcbody
		     (left-paren right-paren block end)
		     (left-paren parlist right-paren block end))
		    (parlist
		     (namelist comma threedots)
		     namelist
		     threedots)
		    (tableconstructor
		     (left-curly right-curly)
		     (left-curly fieldlist right-curly))
		    (fieldlist
		     field
		     (field fieldsep fieldlist)
		     (field fieldsep))
		    (fieldsep
		     comma
		     semicolon)
		    (binop + - * / ^ % twodots < <= > >= == ~=)
		    (unop - not hashtag))

(defun parse-lua (string)
  (parse-with-lexer (lua-lexer string) *lua-parser*))

(in-package :cl-user)
(defun parse-lua (string)
  (cl-lua:parse-lua string))