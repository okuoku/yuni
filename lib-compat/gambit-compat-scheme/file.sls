(library (gambit-compat-scheme file)
         (export
           call-with-input-file call-with-output-file
           delete-file file-exists?
           open-binary-input-file open-binary-output-file
           open-input-file open-output-file
           with-input-from-file with-output-to-file
           )
         (import)

(define-primitive-names/yunifake
  call-with-input-file call-with-output-file
  delete-file file-exists?
  open-input-file open-output-file
  with-input-from-file with-output-to-file)

(define open-binary-input-file 'YUNIFAKE-UNIMPLEMENTED)
(define open-binary-output-file 'YUNIFAKE-UNIMPLEMENTED)
         
)
