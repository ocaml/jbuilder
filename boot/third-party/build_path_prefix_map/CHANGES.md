v0.2 2017-12-10
---------------

- I wrote a Crowbar fuzzer and found a minor issue with it when
  testing roundtrip conversions: `decode_prefix` would accept prefixes
  with `:` character, which breaks the roundtrip property. Also from
  Marrakech.
  (Gabriel Scherer)

v0.1 2017-12-04
---------------

- Initial implementation, motivated by Ximin Luo during the Mirage
  retreat in Marrakech.
  (Gabriel Scherer)
