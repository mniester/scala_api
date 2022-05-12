package Strings

import java.time.LocalDateTime
import pdi.jwt.{Jwt, JwtAlgorithm}

import Settings._


trait isStringNumber {
  def isStringNumber(string: String): Boolean = {
    string.forall(Character.isDigit)
  }
}

trait isStringBoolean {
  def isStringBoolean(string: String): Boolean = {
    if ((string == "true") || (string == "false")) {true} else {false}
  }
}

trait checkISOTimeFormat {
  def checkISOTimeFormat (string: String): Boolean =
    try {
      LocalDateTime.parse(string)
      true
    }
    catch {
      case _: Throwable => false
    }
}


object JWTCoder {
  private val jwtkey = Settings.JWTKey
  private val alg = JwtAlgorithm.HS256
  
  def encode (data: String) = {
    Jwt.encode(data, jwtkey, alg)
  }

  def decode (token: String) = {
    Jwt.decodeRawAll(token, jwtkey, Seq(alg))
  }

  def getInput(token: String) = {
    decode(token).get._2
  }
}