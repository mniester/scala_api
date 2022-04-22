package Strings

import pdi.jwt.{Jwt, JwtAlgorithm}

import Settings._

object JwtCoder {
  private val jwtkey = Settings.JWTKey
  private val alg = JwtAlgorithm.HS256
  
  def encode (token: String) = {
    Jwt.encode(token, jwtkey, alg)
  }

  def decode (token: String) = {
    Jwt.decodeRawAll(token, jwtkey, Seq(alg))
  }
}