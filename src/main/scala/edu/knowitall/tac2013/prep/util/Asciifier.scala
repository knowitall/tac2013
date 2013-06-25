package edu.knowitall.tac2013.prep.util

import java.text.Normalizer

object Asciifier {
  // hacked to return the string unmodified or partially cleaned in case the length changes -
  // Normalizer seems to change the length sometimes.
  def apply(string: String): String = {
    var cleaned = string
      for ((unicode, ascii) <- substitutions) {
        cleaned = cleaned.replaceAll(unicode, ascii)
      }

    if (cleaned.length != string.length) return string
    
    // convert diacritics to a two-character form (NFD)
    // http://docs.oracle.com/javase/tutorial/i18n/text/normalizerapi.html
    val normalized = Normalizer.normalize(cleaned, Normalizer.Form.NFD)

    if (normalized.length != cleaned.length) return cleaned
    
    // remove all characters that combine with the previous character
    // to form a diacritic.  Also remove control characters.
    // http://docs.oracle.com/javase/6/docs/api/java/util/regex/Pattern.html
    val decombined = normalized.replaceAll("[\\p{InCombiningDiacriticalMarks}\\p{Cntrl}]", "")

    if (decombined.length != normalized.length) return normalized else return decombined
  }

  val substitutions = Set(
      (0x00AB, '"'),
      (0x00AD, '-'),
      (0x00B4, '\''),
      (0x00BB, '"'),
      (0x00F7, '/'),
      (0x01C0, '|'),
      (0x01C3, '!'),
      (0x02B9, '\''),
      (0x02BA, '"'),
      (0x02BC, '\''),
      (0x02C4, '^'),
      (0x02C6, '^'),
      (0x02C8, '\''),
      (0x02CB, '`'),
      (0x02CD, '_'),
      (0x02DC, '~'),
      (0x0300, '`'),
      (0x0301, '\''),
      (0x0302, '^'),
      (0x0303, '~'),
      (0x030B, '"'),
      (0x030E, '"'),
      (0x0331, '_'),
      (0x0332, '_'),
      (0x0338, '/'),
      (0x0589, ':'),
      (0x05C0, '|'),
      (0x05C3, ':'),
      (0x066A, '%'),
      (0x066D, '*'),
      (0x200B, ' '),
      (0x2010, '-'),
      (0x2011, '-'),
      (0x2012, '-'),
      (0x2013, '-'),
      (0x2014, '-'),
      (0x2015, '-'),
      (0x2016, '|'),
      (0x2017, '_'),
      (0x2018, '\''),
      (0x2019, '\''),
      (0x201A, ','),
      (0x201B, '\''),
      (0x201C, '"'),
      (0x201D, '"'),
      (0x201E, '"'),
      (0x201F, '"'),
      (0x2032, '\''),
      (0x2033, '"'),
      (0x2034, '\''),
      (0x2035, '`'),
      (0x2036, '"'),
      (0x2037, '\''),
      (0x2038, '^'),
      (0x2039, '<'),
      (0x203A, '>'),
      (0x203D, '?'),
      (0x2044, '/'),
      (0x204E, '*'),
      (0x2052, '%'),
      (0x2053, '~'),
      (0x2060, ' '),
      (0x20E5, '\\'),
      (0x2212, '-'),
      (0x2215, '/'),
      (0x2216, '\\'),
      (0x2217, '*'),
      (0x2223, '|'),
      (0x2236, ':'),
      (0x223C, '~'),
      (0x2264, '<'),
      (0x2265, '>'),
      (0x2266, '<'),
      (0x2267, '>'),
      (0x2303, '^'),
      (0x2329, '<'),
      (0x232A, '>'),
      (0x266F, '#'),
      (0x2731, '*'),
      (0x2758, '|'),
      (0x2762, '!'),
      (0x27E6, '['),
      (0x27E8, '<'),
      (0x27E9, '>'),
      (0x2983, '{'),
      (0x2984, '}'),
      (0x3003, '"'),
      (0x3008, '<'),
      (0x3009, '>'),
      (0x301B, ']'),
      (0x301C, '~'),
      (0x301D, '"'),
      (0x301E, '"'),
      (0xFEFF, ' ')).map { case (unicode, ascii) => (unicode.toChar.toString, ascii.toString) }
}