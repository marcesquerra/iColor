module Utils

public export
interface FromStr a err | a where
  parse: String -> Either err a
