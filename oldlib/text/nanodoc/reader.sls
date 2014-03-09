(library (yuni text nanodoc reader)
         (export nanodoc-parse
                 nanodoc->sexp)
         (import (rnrs))
;;
;; Nanodoc is stripped-down / enhanced version of Markdown.
;; The biggest difference is *NO* HTML TAGS/BLOCKS. We might need to implement 
;; some 'generic' markdown if we want to generate generic HTML pages.
;;
;;  ND = Nanodoc private metadata format
;;  SM = Standard markdown
;; GFM = Github flavored markdown 
;;       ( https://help.github.com/articles/github-flavored-markdown )
;; PME = PHP Markdown Extra ( http://michelf.ca/projects/php-markdown/extra/ )
;;
;; (Backslash escapes:)
;; 
;; \ ` * _ {} [] () # + - . !              (SM)
;; %                                       (ND)
;;
;; (Simple elements: All defined in SM)
;; *emphasis*    ===> (em "emphasis")
;;                    Also: _
;; **strong**    ===> (strong "strong")
;;                    Also: __
;; `code`        ===> (code "code")
;; 
;; # Index       ===> (h1 "Index")
;; ## Index      ===> (h2 "Index")
;;  :
;;  :
;; ###### Index  ===> (h6 "Index")
;;
;; > blockquote  ===> (blockquote ....)
;;
;; 1.            ===> (ol (li "Item") ...)
;; *             ===> (ul (li "Item") ...)
;;                    Also: - +
;;
;; (Link elements: All defined in SM)
;;
;; [Link text](URL "Link text")
;; [Link text][id]
;;
;; (Fenced code blocks: GFM)
;;
;; ```LANG
;; code
;; ```           ===> (code LANG ...)
;;                    LANG will be #f when language was not specified
;;                    LANG: text, c, cxx, scm, scheme, nmosh
;; (Link definitions: SM)
;;
;; [id]: URL "Title Text"
;;
;; (Inline metadata: ND)
;; !{c}                      ===> (lang c)
;; !{scm}                    ===> (lang scheme)
;;
;; (Document metadata: ND)
;;
;; % comment     ===> ignored. One or more space is required.
;; 
;; %env libraries            ===> (env library ...)
;; %env-new libraries        ===> (env-new)(env ...)
         
         
)
