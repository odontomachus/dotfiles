(require 'url-util)

(setq my-xwiki-dist-home "~/projects/hbp/xwiki/xwiki-distribution-collaboratory")
(setq my-xwiki-ext-collab-home (concat (file-name-as-directory my-xwiki-dist-home) "xwiki-ext-collab"))
(setq my-xwiki-data "~/.cache/xwiki")
(setq my-tomcat-root "~/projects/vendor/tomcat/apache-tomcat-9.0.14/webapps/ROOT")

(defun reset-xwiki-perm-dir ()
  (interactive)
  (if (yes-or-no-p "Wipe permanent dir? ")
      (progn
        (delete-directory my-xwiki-data t)
        (make-directory my-xwiki-data)
        )))

(defun compile-ext-collab ()
  (interactive)
(let ((default-directory "~/projects/hbp/xwiki/xwiki-distribution-collaboratory/xwiki-ext-collab"))
  (async-shell-command "mvn -o -Pxwiki compile -Dxwiki.checkstyle.skip=true -Dxwiki.revapi.skip=true -Dxwiki.enforcer.skip=true -Dtest=none -DfailIfNoTests=false -Dxwiki.spoon.skip=true"))
)

(defun my-xwiki-ext-jar-version ()
      (replace-regexp-in-string "\n$" ""
                                ;; Heuristic because using maven takes 3 seconds to get the version.
                                (shell-command-to-string
                                 (concat "grep '<version>' " (file-name-as-directory my-xwiki-ext-collab-home) "pom.xml" " |head -1 | grep -o -P '\\d+\\.\\d+\\.\\d+(-\\w+)?'"))))

;; Return path to jar
(defun my-xwiki-ext-jar (version)
  (concat (file-name-as-directory my-xwiki-ext-collab-home) "target/xwiki-ext-collab-" version ".jar")
  )

;; Return path to jar
(defun my-xwiki-ext-xed (version)
  (concat (file-name-as-directory my-xwiki-ext-collab-home) "target/xwiki-ext-collab-" version ".xed")
  )


(setq url-java-unreserved-chars (vector nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 45 nil nil 48 49 50 51 52 53 54 55 56 57 nil nil nil nil nil nil nil 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 nil nil nil nil 95 nil 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 nil nil nil nil))

(defun xwiki-reload-tomcat ()
  (interactive)
  (shell-command (concat "touch " my-tomcat-root "/META-INF/context.xml"))
  )

(setq url-java-unreserved-chars (vector nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil '- nil '/ '0 '1 '2 '3 '4 '5 '6 '7 '8 '9 nil nil nil nil nil nil nil 'A 'B 'C 'D 'E 'F 'G 'H 'I 'J 'K 'L 'M 'N 'O 'P 'Q 'R 'S 'T 'U 'V 'W 'X 'Y 'Z nil nil nil nil '_ nil 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n 'o 'p 'q 'r 's 't 'u 'v 'w 'x 'y 'z nil nil nil nil))

(defun publish-ext-collab ()
  (interactive)
  (let* ((ext-version (my-xwiki-ext-jar-version))
         (xedpath (my-xwiki-ext-jar ext-version))
         (jarpath (my-xwiki-ext-xed ext-version))
         (dest (concat my-tomcat-root "/WEB-INF/lib/"))
          )
    (progn
        (shell-command (concat "cp " xedpath " " jarpath " " dest))
        (xwiki-reload-tomcat)
        )))

(defun xwiki-update-components ()
  (interactive)
  (let ((default-directory "~/projects/hbp/xwiki/xwiki-distribution-collaboratory/xwiki-ext-collab"))
    (shell-command "grep -Rnl @Component src/main/java |grep 'eu/hbp.*\\.java$' |sed 's@.*/\\(eu.*\\)\\.java@\\1@' |sed 's@/@.@g'|sort > src/main/resources/META-INF/components.txt")
    ))

(provide 'xwiki)
