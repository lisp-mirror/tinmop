(in-package :idn)

(pkg-config-cflags "libidn2")

(include "idn2.h")

(cenum (idn2-rc)
       ((:ok                       "IDN2_OK"))
       ((:malloc                   "IDN2_MALLOC"))
       ((:no-codeset               "IDN2_NO_CODESET"))
       ((:iconv-fail               "IDN2_ICONV_FAIL"))
       ((:encoding-error           "IDN2_ENCODING_ERROR"))
       ((:nfc                      "IDN2_NFC"))
       ((:punycode-bad-input       "IDN2_PUNYCODE_BAD_INPUT"))
       ((:punycode-big-output      "IDN2_PUNYCODE_BIG_OUTPUT"))
       ((:punycode-overflow        "IDN2_PUNYCODE_OVERFLOW"))
       ((:too-big-domain           "IDN2_TOO_BIG_DOMAIN"))
       ((:too-big-label            "IDN2_TOO_BIG_LABEL"))
       ((:invalid-alabel           "IDN2_INVALID_ALABEL"))
       ((:ualabel-mismatch         "IDN2_UALABEL_MISMATCH"))
       ((:invalid-flags            "IDN2_INVALID_FLAGS"))
       ((:not-nfc                  "IDN2_NOT_NFC"))
       ((:2hyphen                  "IDN2_2HYPHEN"))
       ((:hyphen-startend          "IDN2_HYPHEN_STARTEND"))
       ((:leading-combining        "IDN2_LEADING_COMBINING"))
       ((:disallowed               "IDN2_DISALLOWED"))
       ((:contextj                 "IDN2_CONTEXTJ"))
       ((:contextj-no-rule         "IDN2_CONTEXTJ_NO_RULE"))
       ((:contexto                 "IDN2_CONTEXTO"))
       ((:contexto-no-rule         "IDN2_CONTEXTO_NO_RULE"))
       ((:unassigned               "IDN2_UNASSIGNED"))
       ((:bidi                     "IDN2_BIDI"))
       ((:dot-in-label             "IDN2_DOT_IN_LABEL"))
       ((:invalid-transitional     "IDN2_INVALID_TRANSITIONAL"))
       ((:invalid-nontransitional  "IDN2_INVALID_NONTRANSITIONAL")))

;;; this value does not exists for old version of the library, removing
;;; to allow compilation on old system
;;       ((:alabel-roundtrip-failed  "IDN2_ALABEL_ROUNDTRIP_FAILED")))

(cenum (flags)
       ((:nfc-input             "IDN2_NFC_INPUT"))
       ((:alabel-roundtrip      "IDN2_ALABEL_ROUNDTRIP"))
       ((:transitional          "IDN2_TRANSITIONAL"))
       ((:nontransitional       "IDN2_NONTRANSITIONAL"))
       ((:allow-unassigned      "IDN2_ALLOW_UNASSIGNED"))
       ((:use-std3-ascii-rules  "IDN2_USE_STD3_ASCII_RULES"))
       ((:no-tr46               "IDN2_NO_TR46")))

;;; this value does not exists for old version of the library, removing
;;; to allow compilation on old system
;;       ((:no-alabel-roundtrip   "IDN2_NO_ALABEL_ROUNDTRIP")))
