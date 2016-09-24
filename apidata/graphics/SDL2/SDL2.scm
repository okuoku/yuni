(stubir0
  SDL2
  (config
    (stubs (c "SDL2.stub.c")))

  (prologue
    (cpp-include "SDL.h"))

  (types
    (pointer void* (pointer-of void) internal)
    (pointer cvoid* "const void*" (pointer-of void) internal)
    (pointer char* (pointer-of char) internal)
    (pointer char** (pointer-of char*) internal)
    (pointer cchar* "const char*" (pointer-of char) internal)
    (pointer cchar** "const char**" (pointer-of cchar*) internal)
    (pointer size_t* (pointer-of size_t) internal)
    ;; (root)
    (flag-group sdl-init-flags
                (members
                  SDL_INIT_TIMER
                  SDL_INIT_AUDIO
                  SDL_INIT_VIDEO
                  SDL_INIT_JOYSTICK
                  SDL_INIT_HAPTIC
                  SDL_INIT_GAMECONTROLLER
                  SDL_INIT_EVENTS
                  SDL_INIT_NOPARACHUTE
                  SDL_INIT_EVERYTHING))
    ;; main
    ;; stdinc
    (integer SDL_bool)
    (enum-group SDL_bool
                (members SDL_FALSE
                         SDL_TRUE))
    (integer Sint8)
    (unsigned-integer Uint8)
    (integer Sint16)
    (unsigned-integer Uint16)
    (integer Sint32)
    (unsigned-integer Uint32)
    (integer Sint64)
    (unsigned-integer Uint64)
    (enum-group sdl-iconv-error
                (members SDL_ICONV_ERROR
                         SDL_ICONV_E2BIG
                         SDL_ICONV_EILSEQ
                         SDL_ICONV_EINVAL))
    (pointer SDL_iconv_t)
    ;; assert
    ;; atomic
    ;; audio
    ;; clipboard
    ;; cpuinfo
    ;; endian
    ;; error
    ;; events
    ;; filesystem
    ;; joystick
    ;; gamecontroller
    ;; haptic
    ;; hints
    ;; loadso
    ;; log
    ;; messagebox
    ;; mutex
    ;; power
    ;; render
    ;; rwops
    ;; system
    ;; thread
    ;; timer
    ;; version
    ;; video
    
    )
  (layouts
    ;; (root)
    ;; main
    ;; stdinc
    ;; assert
    ;; atomic
    ;; audio
    ;; clipboard
    ;; cpuinfo
    ;; endian
    ;; error
    ;; events
    ;; filesystem
    ;; joystick
    ;; gamecontroller
    ;; haptic
    ;; hints
    ;; loadso
    ;; log
    ;; messagebox
    ;; mutex
    ;; power
    ;; render
    ;; rwops
    ;; system
    ;; thread
    ;; timer
    ;; version
    ;; video
    
    )
  (functions
    ;; (root)
    (int SDL_Init ((Uint32 flags)))
    (int SDL_InitSubSystem ((Uint32 flags)))
    (void SDL_QuitSubSystem ((Uint32 flags)))
    (int SDL_WasInit ((Uint32 flags)))
    (void SDL_Quit ())
    ;; main
    ; SDL_main  ## no sense
    (void SDL_SetMainReady ())
    ; SDL_RegisterApp
    ; SDL_UnregisterApp
    ; SDL_WinRTRunApp
    ;; stdinc
    (void* SDL_malloc ((size_t size)))
    (void* SDL_calloc ((size_t nmemb) (size_t size)))
    (void* SDL_realloc ((void* mem) (size_t size)))
    (void SDL_free ((void* mem)))
    (char* SDL_getenv ((cchar* name)))
    (int SDL_setenv ((cchar* name) (cchar* value) (int overwrite)))
    ; SDL_qsort
    (int SDL_abs ((int x)))
    (int SDL_isdigit ((int x)))
    (int SDL_isspace ((int x)))
    (int SDL_toupper ((int x)))
    (int SDL_tolower ((int x)))
    (void* SDL_memset ((void* dst) (int c) (size_t len)))
    (void SDL_memset4 ((void* dst) (Uint32 val) (size_t dwords)))
    (void* SDL_memcpy ((void* dst) (cvoid* src) (size_t len)))
    (void* SDL_memcpy4 ((void* dst) (cvoid* src) (size_t dwords)))
    (void* SDL_memmove ((void* dst) (cvoid* src) (size_t len)))
    (int SDL_memcmp ((cvoid* s1) (cvoid* s2) (size_t len)))
    ; SDL_wcslen
    ; SDL_wcslcpy
    ; SDL_wcslcat
    (size_t SDL_strlen ((cchar* str)))
    (size_t SDL_strlcpy ((char* dst) (cchar* src) (size_t maxlen)))
    (size_t SDL_utf8strlcpy ((char* dst) (cchar* src) (size_t dst_bytes)))
    (size_t SDL_strlcat ((char* dst) (cchar* src) (size_t maxlen)))
    (char* SDL_strdup ((cchar* str)))
    (char* SDL_strrev ((char* str)))
    (char* SDL_strupr ((char* str)))
    (char* SDL_strlwr ((char* str)))
    (char* SDL_strchr ((cchar* str) (int c)))
    (char* SDL_strrchr ((cchar* str) (int c)))
    (char* SDL_strstr ((cchar* haystack) (cchar* needle)))
    (char* SDL_itoa ((int value) (char* str) (int radix)))
    (char* SDL_uitoa ((unsigned-int value) (char* str) (int radix)))
    (char* SDL_ltoa ((long value) (char* str) (int radix)))
    (char* SDL_ultoa ((unsigned-long value) (char* str) (int radix)))
    (char* SDL_lltoa ((Sint64 value) (char* str) (int radix)))
    (char* SDL_ulltoa ((Uint64 value) (char* str) (int radix)))
    (int SDL_atoi ((cchar* str)))
    (double SDL_atof ((cchar* str)))
    (long SDL_strtol ((cchar* str) (char** endp) (int base)))
    (unsigned-long SDL_strtoul ((cchar* str) (char** endp) (int base)))
    (Sint64 SDL_strtoll ((cchar* str) (char** endp) (int base)))
    (Uint64 SDL_strtoull ((cchar* str) (char** endp) (int base)))
    (double SDL_strtod ((cchar* str) (char** endp)))
    (int SDL_strcmp ((cchar* str1) (cchar* str2)))
    (int SDL_strncmp ((cchar* str1) (cchar* str2) (size_t maxlen)))
    (int SDL_strcasecmp ((cchar* str1) (cchar* str2)))
    (int SDL_strncasecmp ((char* str1) (cchar* str2) (size_t len)))
    ; SDL_sscanf
    ; SDL_vsscanf
    ; SDL_snprintf
    ; SDL_vsnprintf
    (double SDL_acos ((double x)))
    (double SDL_asin ((double x)))
    (double SDL_atan ((double x)))
    (double SDL_atan2 ((double x) (double y)))
    (double SDL_ceil ((double x)))
    (double SDL_copysign ((double x) (double y)))
    (double SDL_cos ((double x)))
    (float SDL_cosf ((float x)))
    (double SDL_fabs ((double x)))
    (double SDL_floor ((double x)))
    (double SDL_log ((double x)))
    (double SDL_pow ((double x) (double y)))
    (double SDL_scalbn ((double x) (int n)))
    (double SDL_sin ((double x)))
    (float SDL_sinf ((double x)))
    (double SDL_sqrt ((double x)))
    (SDL_iconv_t SDL_iconv_open ((cchar* tocode) (cchar* fromcode)))
    (int SDL_iconv_close ((SDL_iconv_t cd)))
    (size_t SDL_iconv ((SDL_iconv_t cd) 
                       (cchar** inbuf) (size_t* inbytesleft)
                       (char** outbuf) (size_t* outbytesleft)))
    (char* SDL_iconv_string ((cchar* tocode) (cchar* fromcode)
                                             (cchar* inbuf)
                                             (size_t inbytesleft)))
    ;; assert
    ;; atomic
    ;; audio
    ;; clipboard
    ;; cpuinfo
    ;; endian
    ;; error
    ;; events
    ;; filesystem
    ;; joystick
    ;; gamecontroller
    ;; haptic
    ;; hints
    ;; loadso
    ;; log
    ;; messagebox
    ;; mutex
    ;; power
    ;; render
    ;; rwops
    ;; system
    ;; thread
    ;; timer
    ;; version
    ;; video
    
    )
  (exports
    ;; (root)
    ;; main
    (int SDL_MAIN_NEEDED macro)
    ;; stdinc
    ;; assert
    ;; atomic
    ;; audio
    ;; clipboard
    ;; cpuinfo
    ;; endian
    ;; error
    ;; events
    ;; filesystem
    ;; joystick
    ;; gamecontroller
    ;; haptic
    ;; hints
    ;; loadso
    ;; log
    ;; messagebox
    ;; mutex
    ;; power
    ;; render
    ;; rwops
    ;; system
    ;; thread
    ;; timer
    ;; version
    ;; video
    
    )
  
  )
