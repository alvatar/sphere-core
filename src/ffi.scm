(define get-size-of-unsigned-char
  (c-lambda () int "___result = sizeof(unsigned char);"))

(define void*->unsigned-char*
  (c-lambda ((pointer void)) (pointer unsigned-char)
            "___result_voidstar = ___arg1;"))

(define calloc
  (c-lambda (int int) (pointer void) "calloc"))

(define free
  (c-lambda ((pointer void)) void "free"))