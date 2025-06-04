package Wedding


/** Wedding planning Project 👰🏽‍ ⛪️ 🧁 consolidates Cohort 2 Academy lessons * */


import scala.collection.mutable.Map
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Random, Success}
import java.time.{LocalDate, Duration}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{Duration, FiniteDuration}


object WeddingPlan extends App { //Wk 2


  val saveTheDate: String = "Save the date for the wedding of May and Tom!" //Wk 1

  val bridesmaids: Seq[String] = Seq("Sarah", "Lisa", "Victoria") //Wk 1

  /** Countdown to the day⏱️ */
  val weddingDate = LocalDate.of(2025, 11, 11)
  val today = LocalDate.now()
  val daysUntilWedding = java.time.temporal.ChronoUnit.DAYS.between(today, weddingDate)

  println(s" The countdown is on to the 11th of November 2025! There's $daysUntilWedding days until the wedding of May & Tom! 🎉")


  //Plan wedding themes
  sealed trait WeddingTheme {
    def name: String

    def description: String

    def venueType: String

    def outdoorSpace: List[String]

    def churchOnsite: Boolean

    def churchCapacity: Int
  }

  case object GatsbyTheme extends WeddingTheme {
    val name = "Gatsby"
    val description = "1930's splendour, cocktails and glamour"
    val venueType = "Stately home"
    val outdoorSpace = List("Front lawn for reception drinks", "Moat area for photographs")
    val churchOnsite = true
    val churchCapacity = 80
  }

  case object CountrysideTheme extends WeddingTheme {
    val name = "Quintessentially English Countryside chic"
    val description = "country house within acres of parkland, walk form church to venue down rolling hills"
    val venueType = "country home"
    val outdoorSpace = List("Front lawn", "back lawn", "path area")
    val churchOnsite = true
    val churchCapacity = 75
  }

  case object BeachTheme extends WeddingTheme {
    val name = "beach glamour"
    val description = "Devon coastal retreat"
    val venueType = "Beach vibes"
    val outdoorSpace = List("back terrace")
    val churchOnsite = false
    val churchCapacity = 0
  }

  val allThemes = List(GatsbyTheme, CountrysideTheme, BeachTheme)

  def venuesWithChurch: List[WeddingTheme] = {
    allThemes.filter(_.churchOnsite)
  }

  println("Venues with church:") // List of venues with church //Gatsby & Quintessentially English Countryside chic
  venuesWithChurch.foreach(theme => println(s"- ${theme.name}"))



  val tableNames: Map[Int, String] = Map( //Wk 1 + mutable Map without making tableNames a var
    1 -> "Oxford Street",
    2 -> "Victoria",
    3 -> "Clapham Common",
    4 -> "Baker Street",
    5 -> "Brixton",
    6 -> "Westminster",
    7 -> "Stockwell",
    8 -> "London Bridge",
    9 -> "Vauxhall",
    10 -> "Westminster"
  )
  val filterForTableOxfordStreet: Map[Int, String] = tableNames.filter(num => num._1 == 1)
  println(filterForTableOxfordStreet) //HashMap(1 -> Oxford Street)

  /** We have more guests! add another table number 11 called "Pimlico" * */
  tableNames += (11 -> "Pimlico")
  println(tableNames) //shows 11 table names now not 10



  val cakeFlavour = List("Profitterol Tower", "2 tier", "traditional 3 tier")
  val cakeTopper = List("Edible flowers", "Acrylic Butterflies", "Rice paper flowers")
  val cakeDecoration = List("Sprinkles", "Fresh fruit", "chocolate balls", "chocolate and cream")

  val cakePlanning = for { //for comp Wk 1
    style <- cakeFlavour
    topOfTheCake <- cakeTopper
    decoration <- cakeDecoration
  } yield s"${style} with ${topOfTheCake} covered in ${decoration} for the wedding!"

  println(cakePlanning(3)) //a combination
  println("Here are your top 3 wedding cakes ideas:")
  cakePlanning.take(3).foreach(println)



  //Wk 3
  object PhotographySlots extends Enumeration {
    val BrideGettingReady = Value(0)
    val BrideFamily = Value(1)
    val GroomFamily = Value(2)
    val AtChurch = Value(3)
    val ConfettiThrow = Value(4)
    val AtReception = Value(5)
    val Speeches = Value(6)
    val CakeCutting = Value(7)
  }

  def photographyTime(slot: PhotographySlots.Value): Int = slot match {
    case PhotographySlots.BrideGettingReady => 90 //minutes
    case PhotographySlots.BrideFamily => 30
    case PhotographySlots.GroomFamily => 30
    case PhotographySlots.AtChurch => 40
    case PhotographySlots.ConfettiThrow => 15
    case PhotographySlots.AtReception => 45
    case PhotographySlots.Speeches => 90
    case PhotographySlots.CakeCutting => 15
  }

  println(photographyTime(PhotographySlots.AtChurch)) //40 (mins)
  println(s"Allocated time at Church is ${photographyTime(PhotographySlots.AtChurch)} minutes.") //Allocated time at Church is 40 minutes.



  //Wk 2
  case class Wedding(bride: Person, groom: Person, reception: Reception, guests: List[Guest])

  case class Venue(name: String, address: String, capacity: Int, costPerDay: Double, churchOnSite: Boolean)

  object Venue {
    def createVenue(name: String, address: String, capacity: Int): Venue = {
      Venue(name, address, capacity, 3500, true)
    }
  }

  val firstChoiceVenue = Venue.createVenue("Abbots Hall", "123 Sidney Street, Ipswich", 310)
  println(firstChoiceVenue)
  val secondChoiceVenue = Venue.createVenue("Cranburg Castle", "6a Pine Street, Cambridge", 550)
  println(secondChoiceVenue)


  case class Reception(venue: Venue, mealOption: List[String]) //Wk 2

  //Person class of anyone involved in wedding
  case class Person(name: String, email: String, phoneNumber: Option[String] = None) //Wk 3

  case class Guest(person: Person, plusOne: Option[Guest] = None, dietaryRequirements: List[String] = List())

  //Instance of a bride and groom
  val bride: Person = Person("May Green", "May@me.com")
  val groom: Person = Person("Tom Brown", "Tom@me.co.uk")

  val aboutBride = s"The bride's name is ${bride.name} and her email is ${bride.email}" //Wk 2
  println(aboutBride)

  //Create some guests
  val tod = Guest(person = Person("Tod Maine", "tod@btinternet.com", Some("07790116679")), plusOne = Some(annie), dietaryRequirements = List())
  val sam = Guest(person = Person("Sam Heart", "sb2340@yahoo.com", Some("079901161123")), plusOne = Some(tim), dietaryRequirements = List("Vegan"))

  //Create some plus One guests - we don't always need guest phone numbers.
  val annie = Guest(person = Person("Annie Plum", "annie@example.com"), dietaryRequirements = List("Vegeterian"))
  val tim = Guest(person = Person("Tim Bolt", "tb@yahoo.com"), dietaryRequirements = List("None"))
  println(tod)
  println(tim)

  val guests = List(tod, sam, annie, tim)
  //Wk 4
  //  val VegeterianGuests = Guest.filter(guest => guest.dietaryRequirements.contains("Vegeterian"))
  //println("Vegeterian Guest names:")
  // vegeterianGuests.foreach(guest => println(guest.person.name)

  val vegeterianGuests = for {
    guests <- guests if guests.dietaryRequirements.contains ("Vegeterian")
  }
  yield guests
println(s"The veg guests are: ${vegeterianGuests}")



  class WeddingPlanner( //Wk2
                        val name: String,
                        val company: String,
                        val contactNumber: String,
                        val chosenDesign: String,
                        val weddingBudget: Double)

  val weddingPlanner = new WeddingPlanner("Crimson Gretal", "JB & Co", "07778900900", "Tradional Elegance", 12000.0)
  println(s"The wedding planners name is ${weddingPlanner.name}.")
  println(s"The wedding planners budget is £${weddingPlanner.weddingBudget}.")




  /** Welcome visitors from Wales, England and Spain to the reception in their language */
  def welcome(language: String): String = language.toLowerCase
  match {
    case "english" => "Welcome to the wedding of May & Tom"
    case "spanish" => "Bienvenido a la boda de May & Tom"
    case "welsh" => "Croeso i briodas May ac Tom"
    case _ => "Welcome" // Default to English if not found
  }
  println(welcome("welsh")) //Croeso



  /** Venue hire rates (discount applies to hiring it for 3 days) */
  def cost(days: Int): Int = { //Wk 1 & 2
    val dailyVenueHire = 3500
    val initialCost = dailyVenueHire * days

    if (days >= 3) {
      initialCost - 450
    } else if (days >= 1) {
      initialCost - 0
    }
    else
      initialCost
  }

  println(cost(3))
  println(cost(2))



  //Wk 3 Eithers
  def bookVenue(venue: String): Either[String, String] = {
    try {
      if (venue == "The Plaza NYC") {
        throw new Exception("Venue is not available to be be booked for wedding")
      } else {
        Right(s"$venue has been booked for the wedding!")
      }
    } catch {
      case e: Exception =>
        Left(e.getMessage)
    }
  }

  val venue = "The Plaza NYC"

  println(bookVenue(venue).fold(
    error => s"Error: $error",
    success => success
  ))
  println(bookVenue("The Plaza NYC"))



  def DrinkChoices(age: Int): String = age match {
    case age if age < 5 => "babychino"
    case age if age >= 5 && age < 9 => "Appletizer"
    case age if age >= 9 && age < 18 => "Shirley Temple Mocktail"
    case _ => "Champagne or Dark and Stormy Cocktail"
  }

  println(s"We will serve the following for a guest aged 10: ${DrinkChoices(10)}")



  //Wk 3
  val firstChoiceFlowers: Option[String] = Some("Blush Roses")
  //val firstChoiceFlowers: Option[String] = None
  val bridesmaidFlowers = firstChoiceFlowers.getOrElse("Pink Sweet Pea")
  println(s"The bridesmaids flowers are ${bridesmaidFlowers}")



  //Wk 4 Future
  def brideAndGroomEntrance: Future[String] = {
    Future {
      Thread.sleep(2000)
      "Please all stand and raise your glasses to the bride and groom!🥂"
    }
  }

  val Entrance = brideAndGroomEntrance
  Entrance.onComplete {
    case Success(result) => println(result)
    case Failure(exception) => println(s"Failure with the brides and grooms entrance: ${exception.getMessage}")
  }
  Thread.sleep(2000)



  //Wk 4 Recursion
  def tryWeddingDresses(dresses: List[String]): Unit = {
    def TryADress(remainingDresses: List[String]): Unit = {
      if (remainingDresses.isEmpty) {
        println("There's no more dresses to try on! End of the fitting appointment.") //base case
      } else {
        println(s"Try on the dress: ${remainingDresses.head}") // Try on first dress
        TryADress(remainingDresses.tail) // Call the function recursively with the rest of the dresses
      }
    }

    TryADress(dresses)
  }

  val dressList = List("Lace fitted dress with cape", "Long-sleeved with veil", "Sweetheart neckline with lace bolero", "Mermaid with floor length veil")
  tryWeddingDresses(dressList)



  val weddingTotalBudget = 12000
  case class Vendors(name: String, itemCost: Int, itemBudget: Int)
  val vendors = List(
    Vendors("Flowers", 790, 750),
    Vendors("Catering", 3200, 3000),
    Vendors("Venue", 7000, 7000),
    Vendors("Band", 1200, 1000),
    Vendors("Crepe Truck", 900, 1000)
  )

  val totalCost = vendors.map(_.itemCost).sum
  val totalItemBudget = vendors.map(_.itemBudget).sum

  println(s"The vendor items total cost is £${totalCost}")
  println(s"The budget for all the vendor items is £${totalItemBudget}")

  //something about adding tax to how much things cost eg flowers
  //say Flowers have a budget of £6000. Flowers cost 750. Can afford?? yes / no



}



//Have a look through my cafe
////flatten


//Wk 4 Hof - write a tahnk you note


// or n//Something about assigning people to a table and match to a table
//Keyset one from udemy course?

//Look at wk 3 Thursday options/Pmatch /chocbar/ pizza
//something about the hog roast may come xyz or not ??

//Extra - variance swan exercise

//total budget spent so far? remaining budget ??

//def confirmedGuests attenidng
//Total invited and how many attending ??
//Update attending list of guests have confimred


//add a trait maybe vendor trait extends person ?? then a case class wedding Vendor
//photographer, florist, etc
//Tracking which budget items are over budget


//Ideas:
//honeymoon destination
//bridemaids dresses

//create invitations / send them
//where people will sit
//check who has RSVPS
//Thankyou notes
//can I get a list of all the guests??






