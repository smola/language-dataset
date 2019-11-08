# Latin1 incompatibility
# While ISO-8859-1 (the widespread Latin1 that is HTML's historical default
# charset) is a code subset of Unicode, ISO-8859-1's 8bit encoding scheme is no
# code subset of UTF-8. Latin1 letters look quite different when transformed into
# UTF-8. Non-ASCII characters in UTF-8 output look illegible on Latin-1 terminals.
# The many existing and unlabeled Latin1 texts are no legal UTF-8 input. Contrary
# to UTF-8, SCSU, JAVA and HTML allow Latin1 text to pass through transparently
# without being limited to Latin1.
# Source: http://www.czyborra.com/utf/

s/Â//g
s/Â¡/¡/g
s/Â¢/¢/g
s/Â£/£/g
s/Â¤/¤/g
s/Â¥/¥/g
s/Â¦/¦/g
s/Â§/§/g
s/Â¨/¨/g
s/Â©/©/g
s/Âª/ª/g
s/Â«/«/g
s/Â¬/¬/g
s/Â­/­/g
s/Â®/®/g
s/Â¯/¯/g
s/Â°/°/g
s/Â±/±/g
s/Â²/²/g
s/Â³/³/g
s/Â´/´/g
s/Âµ/µ/g
s/Â¶/¶/g
s/Â·/·/g
s/Â¸/¸/g
s/Â¹/¹/g
s/Âº/º/g
s/Â»/»/g
s/Â¼/¼/g
s/Â½/½/g
s/Â¾/¾/g
s/Â¿/¿/g
s/Ã€/À/g
s/Ã /Á/g
s/Ã‚/Â/g
s/Ãƒ/Ã/g
s/Ã„/Ä/g
s/Ã…/Å/g
s/Ã†/Æ/g
s/Ã‡/Ç/g
s/Ãˆ/È/g
s/Ã‰/É/g
s/ÃŠ/Ê/g
s/Ã‹/Ë/g
s/ÃŒ/Ì/g
s/Ã /Í/g
s/ÃŽ/Î/g
s/Ã /Ï/g
s/Ã /Ð/g
s/Ã‘/Ñ/g
s/Ã’/Ò/g
s/Ã“/Ó/g
s/Ã”/Ô/g
s/Ã•/Õ/g
s/Ã–/Ö/g
s/Ã—/×/g
s/Ã˜/Ø/g
s/Ã™/Ù/g
s/Ãš/Ú/g
s/Ã›/Û/g
s/Ãœ/Ü/g
s/Ã /Ý/g
s/Ãž/Þ/g
s/ÃŸ/ß/g
s/Ã /à/g
s/Ã¡/á/g
s/Ã¢/â/g
s/Ã£/ã/g
s/Ã¤/ä/g
s/Ã¥/å/g
s/Ã¦/æ/g
s/Ã§/ç/g
s/Ã¨/è/g
s/Ã©/é/g
s/Ãª/ê/g
s/Ã«/ë/g
s/Ã¬/ì/g
s/Ã­/í/g
s/Ã®/î/g
s/Ã¯/ï/g
s/Ã°/ð/g
s/Ã±/ñ/g
s/Ã²/ò/g
s/Ã³/ó/g
s/Ã´/ô/g
s/Ãµ/õ/g
s/Ã¶/ö/g
s/Ã·/÷/g
s/Ã¸/ø/g
s/Ã¹/ù/g
s/Ãº/ú/g
s/Ã»/û/g
s/Ã¼/ü/g
s/Ã½/ý/g
s/Ã¾/þ/g
s/Ã¿/ÿ/g
