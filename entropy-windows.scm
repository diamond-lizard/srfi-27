;;;; entropy-windows.scm
;;;; Kon Lovett, Oct '09

(module entropy-windows

(;export
  make-entropy-source-crypt)

(import scheme chicken foreign)

(use
  (only ports make-input-port)
  (only srfi-4 make-u8vector u8vector-ref)
  entropy-source entropy-port)

;;;

(include "srfi-27-common-types")

;;; Entropy from CryptContext

#>
#include <windows.h>
#include <Wincrypt.h>

static LPVOID lpErrMsgBuf;
static DWORD last_errcod;

static void
set_last_errmsg()
{
  last_errcod = GetLastError();
  FormatMessage(
    FORMAT_MESSAGE_ALLOCATE_BUFFER
    | FORMAT_MESSAGE_FROM_SYSTEM
    | FORMAT_MESSAGE_IGNORE_INSERTS,
    NULL,
    last_errcod,
    MAKELANGID( LANG_NEUTRAL, SUBLANG_DEFAULT ),
    (LPTSTR) &lpErrMsgBuf,
    0, NULL );
}

static char *
get_last_errmsg()
{
  return (char *) lpErrMsgBuf;
}

static void
rel_last_errmsg()
{
  LocalFree( lpErrMsgBuf );
}

static int
get_crypt_prov( unsigned long * hProv )
{
  if( CryptAcquireContext( (HCRYPTPROV *) hProv, NULL, NULL, PROV_RSA_FULL,
                           CRYPT_VERIFYCONTEXT | CRYPT_SILENT ) ) {
    return 1;
  } else {
    set_last_errmsg();
    return 0;
  }
}

static int
fill_rand_buff( unsigned long hProv, uint8_t * buff, int len )
{
  if( CryptGenRandom( (HCRYPTPROV) hProv, len, (unsigned char *) buff ) ) {
    return 1;
  } else {
    set_last_errmsg();
    return 0;
  }
}

static int
rel_crypt_prov( unsigned long hProv )
{
  if( CryptReleaseContext( (HCRYPTPROV) hProv, 0 ) ) {
    return 1;
  } else {
    set_last_errmsg();
    return 0;
  }
}
<#

(define get_crypt_prov (foreign-lambda int "get_crypt_prov" (c-pointer unsigned-long)))
(define fill_rand_buff (foreign-lambda int "fill_rand_buff" unsigned-long u8vector int))
(define rel_crypt_prov (foreign-lambda int "rel_crypt_prov" unsigned-long))
(define get_last_errmsg (foreign-lambda c-string "get_last_errmsg"))
(define rel_last_errmsg (foreign-lambda void "rel_last_errmsg"))

(define (last-err loc msg)
  ;the string is copied into the Scheme heap so can be released
  (let ((errmsg (get_last_errmsg)))
    (rel_last_errmsg)
    (error loc msg errmsg) ) )

(define-syntax chkerr
  (syntax-rules ()
    ((_ ?loc ?res ?msg)
      (unless ?res (last-err ?loc ?msg)) ) ) )

(define-constant DEFAULT-CRYPT-BUFFLEN 64)

(define (open-crypt-random-port buflen)
  (let-location ((hprov unsigned-long))
    (chkerr 'crypt-random-port
      (get_crypt_prov #$hprov) "cannot acquire random provider")
    (let ((buf (make-u8vector buflen))
          (len 0)
          (pos 0) )
      (let ((fillbuff
              (lambda ()
                (chkerr 'crypt-random-port
                  (fill_rand_buff hprov buf buflen) "cannot get random buffer")
                (set! pos 0)
                (set! len buflen) ) )
            (getchar
              (lambda ()
                (let ((ch (u8vector-ref buf pos)))
                  (set! pos (fx+ pos 1))
                  (integer->char ch) ) ) ) )
        ;Binary input port w/o lookahead, string, and line support
        (make-input-port
          (lambda ()                    ;Read
            (when (fx>= pos len) (fillbuff))
            (getchar) )
          (lambda ()                    ;Ready?
            #t )
          (lambda ()                    ;Close
            (chkerr 'crypt-random-port
              (rel_crypt_prov hprov) "cannot release random provider") ) ) ) ) ) )

;;;

(: make-entropy-source-crypt (#!optional fixnum --> entropy-source))
;
(define (make-entropy-source-crypt #!optional (buflen DEFAULT-CRYPT-BUFFLEN))
  (make-entropy-source/port-open
    (lambda () (open-crypt-random-port buflen))
    'crypt
    "Entropy from CryptRandom") )

(register-entropy-source! 'crypt make-entropy-source-crypt) ;only w/ default buffer length

) ;module entropy-windows
