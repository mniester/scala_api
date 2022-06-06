package Coders

import pdi.jwt.{Jwt, JwtAlgorithm}
import scala.util.{Failure, Success}

import Settings._
import ApiMessages.ResponseMessage


object JwtCoder {
  private val jwtkey = Settings.JwtKey
  private val alg = JwtAlgorithm.HS256
  
  def encode (data: String) = {
    Jwt.encode(data, jwtkey, alg)
  }

  def decode (token: String): Option[(String, String, String)] = {
    Jwt.decodeRawAll(token, jwtkey, Seq(alg)) match {
      case Success(s) => Some(s)
      case Failure(f) => None
    }
  }

  def decodeInput (token: String): Option[String] = {
    decode(token).getOrElse(null) match {
      case null => None
      case tuple => Some(tuple._2)
    }
  }
}