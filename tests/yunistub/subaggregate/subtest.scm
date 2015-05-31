(stubir0
  unnamed
  (yunistub testing unnamed)
  (config
    (stubs (c "unnamed.stub.c")))
  (prologue)
  (types
    (integer hoge_e c-enum)
    (blob    hoge_s c-struct)
    (blob    hoge_t)
    (enum-group hoge_e
                (members E_A E_B)))
  (layouts
    (aggregate hoge_s
               (int       m_a)
               (char      m_b)
               (hoge_e    m_c array)
               (aggregate m_d
                          (char mm_a)
                          (int  mm_b))))
  (exports
    (int D macro)))

