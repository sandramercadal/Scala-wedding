package Week1.Wedding

import scala.io.StdIn.readLine

object WeddingChat extends App {

  AsciiArt.printLogo()

  private def greetFutureBride(): Unit = {
    val name: String = readLine("What's your name: ")
    println(s"Hi, $name")
  }

  private def startPlanning(): Unit = {
    val choice: String = readLine("What is your favourite colour? Pink, purple, yellow or blue?"
    ).toLowerCase

    if (choice == "pink")
      println("Your wedding flowers could be Roses, beautiful!")
    else if (choice == "purple")
      println("Great choice! You could get Peonies for your wedding.")
    else if (choice == "yellow")
      println("How happy are you? Pop some Sunflowers in your bouquet.")
    else if (choice == "blue")
      println("Excellent! Mix in some Hydrangeas in your arrangement.")
    else
      println("You didn't make a choice")
  }

  private def chooseTheme(): Unit = {
    val choice: String = readLine("Let's continue this is fun! Now let's choose your wedding Theme! What is your spirit animal: Pantheress, Rabbit, Dolphin or Deer?").toLowerCase
    if (choice == "pantheress")
      println("So luxurious, have a Great Gatsby themed wedding. Think: 1930's splendour, cocktails and glamour in a stately home. Front lawn for reception drinks and moat area for photographs")
    else if (choice == "rabbit")
      println("You give a Quintessentially English Countryside vibe. Book a country house within acres of parkland, walk form church to venue down rolling hills and enjoy a big party in a stately country home.")
    else if (choice == "dolphin")
      println("What a whimsical creature! I am thinking Beach glamour! Book your guests in for a Devon coastal retreat, enjoy cocktails at the beach and dance under the sun.")
    else if (choice == "deer")
      println("Don't we all love a festival? Think Glastonbury: tents, music, fire and dancing all night long with your besties.")
    else
      println("You didn't make a choice")

  }

  greetFutureBride()

  println("Let me help you to plan your perfect wedding, let's start...")

  startPlanning()

  println("I have a few more questions for you...")

  chooseTheme()

}
