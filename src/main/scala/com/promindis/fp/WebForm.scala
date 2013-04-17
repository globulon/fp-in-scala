package com.promindis.fp

import java.util.Date

case class WebForm(name: String, birthdate: Date, phoneNumber: String)

object WebForm {
  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty")

  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {
      import java.text._
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate))
    } catch {
      case e: Throwable => Failure ("Birthdate must be in the form yyyy-MM-dd")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}")) Success(phoneNumber)
    else Failure("Phone number must be 10 digits")


  val applicative = Validation.applicative[String]

  def validWebForm(name: String,
                   birthdate: String,
                   phone: String): Validation[String, WebForm] =
    applicative(applicative(applicative(applicative.unit((WebForm(_,_,_)).curried))(
      validName(name)))(
      validBirthdate(birthdate)))(
      validPhone(phone))

  def main(args: Array[String]) {
    println(validWebForm("", "19710828", "abc"))
  }
}