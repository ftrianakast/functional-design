package net.degoes

import java.time.Instant

import net.degoes.cms.User
import net.degoes.credit_card.DeGoesSolution.Price
import net.degoes.credit_card.PricingScheme
import net.degoes.events.{DeviceEvent, Event, UserEvent}

/*
 * INTRODUCTION
 *
 * Functional Design depends heavily on functional data modeling. Functional
 * data modeling is the task of creating precise, type-safe models of a given
 * domain using algebraic data types and generalized algebraic data types.
 *
 * In this section, you'll review basic functional domain modeling.
 */

/**
 * E-COMMERCE - EXERCISE SET 1
 *
 * Consider an e-commerce application that allows users to purchase products.
 */
object credit_card {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a credit card, which must have:
   *
   *  * Number
   *  * Name
   *  * Expiration date
   *  * Security code
   */
  type CreditCard

  case class CreditCardProduct(number: Int, name: String, expirationDate: Long, securityCode: SecurityCode)

  sealed trait SecurityCode
  object SecurityCode {
    final case class Four(_1: Digit, _2: Digit, _3: Digit, _4: Digit) extends SecurityCode
    final case class Three(_1: Digit, _2: Digit, _3: Digit) extends SecurityCode
  }

  /// This is not very practical but you can use two things
  // 1. Smart constructor
  // 2. Refine library
  sealed trait Digit
  object Digit {
    case object `0` extends Digit
    case object `1` extends Digit
    case object `2` extends Digit
    case object `3` extends Digit
  }

  sealed abstract class SecurityCodeSafe private (value: Int)
  object SecurityCodeSafe {
    // With smart constructor the return type could be Either[CreditCardError, SecurityCode] but we should avoid
    // because option is the minimal representation

    def fromInt(int: Int): Option[SecurityCodeSafe] =
      if(int.toString.length >= 3 && int.toString.length <=4) Some (new SecurityCodeSafe(int){}) else None
  }
  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product, which could be a physical product, such as a gallon of milk,
   * or a digital product, such as a book or movie, or access to an event, such
   * as a music concert or film showing.
   */
  object MySolution {
    //type Product
    sealed trait Product
    sealed trait DigitalProduct extends Product
    object Product {
      final case class GallonOfMilk() extends Product
      final case class Book() extends DigitalProduct
      final case class Movie() extends DigitalProduct
    }
  }

  object TheStore {
    final case class Product(id: String, price: Price, productType: ProductType)

    sealed trait ProductType
    object ProductType {
      final case class Book(isbn: String, title: String, author: String) extends ProductType
      final case class Drink(name: String, quantity: Quantity) extends ProductType
      final case class Movie(title: String, actors: Seq[String]) extends ProductType
    }

    type Quantity
    type Price
  }

  object SomeOtherExample {
    final case class Product(id: String, price: Price, productType: ProductType) { self => {
      def increasePrice: Product = self.copy(price = self.price)
    }}

    sealed trait ProductType
    object ProductType {
      final case class Book(isbn: String, title: String, author: String) extends ProductType
      final case class Drink(name: String, quantity: Quantity) extends ProductType
      final case class Movie(title: String, actors: Seq[String]) extends ProductType
    }

    def increasePrice(product: Product): Product =
      product.copy(price = product.price)

    type Quantity
    type Price
  }


  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product price, which could be one-time purchase fee, or a recurring
   * fee on some regular interval.
   */
  //type PricingScheme
  sealed trait PricingScheme
  object PricingScheme {
    final case class OneTime(amount: Price) extends PricingScheme
    final case class RegularInterval(amount: Price, start: java.time.Instant, interval: java.time.Instant) extends PricingScheme
  }

  object PricingSchemeRefactored {

    case class PricingScheme(amount: Price, pricingRecurrence: PricingRecurrence)
    sealed trait PricingRecurrence
    final case class OneTime(amount: Price) extends PricingRecurrence
    final case class RegularInterval(amount: Price, start: java.time.Instant, interval: java.time.Instant) extends PricingRecurrence
  }
}

/**
 * EVENT PROCESSING - EXERCISE SET 3
 *
 * Consider an event processing application, which processes events from both
 * devices, as well as users.
 */
object events {

  /**
   * EXERCISE
   *
   * Refactor the object-oriented data model in this section to a more
   * functional one, which uses only sealed traits and case classes.
   */
  abstract class Event(val id: Int) {

    def time: Instant
  }

  // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
  // please don't extend both it will break code!!!
  trait UserEvent extends Event {
    def userName: String
  }

  // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
  // please don't extend both it will break code!!!
  trait DeviceEvent extends Event {
    def deviceId: Int
  }

  class SensorUpdated(id: Int, val deviceId: Int, val time: Instant, val reading: Option[Double])
      extends Event(id)
      with DeviceEvent

  class DeviceActivated(id: Int, val deviceId: Int, val time: Instant) extends Event(id) with DeviceEvent

  class UserPurchase(id: Int, val item: String, val price: Double, val time: Instant, val userName: String)
      extends Event(id)
      with UserEvent

  class UserAccountCreated(id: Int, val userName: String, val time: Instant) extends Event(id) with UserEvent


  object FunctionalEvent {

    final case class Event(id: Int, time: Instant, sourcedEvent: SourcedEvent)

    sealed trait SourcedEvent
    final case class DeviceEvent(deviceId: Int, specificEvent: SpecificEvent) extends SourcedEvent
    final case class UserEvent(userName: String) extends SourcedEvent

    sealed trait SpecificEvent
    final case class SensorUpdated(reading: Option[Double]) extends SpecificEvent
    final case class DeviceActivated() extends SpecificEvent
  }

  object otherSolution {
    final case class Event(id: Int, time: Instant, `type`: EventType)

    sealed trait EventType
    object EventType {
      final case class DeviceEvent(deviceId: Int, `type`: DeviceEventType) extends EventType
      final case class UserEvent(userName: String, `type`: UserEventType)  extends EventType
    }

    sealed trait DeviceEventType

    object DeviceEventType {

      case object DeviceActivated                             extends DeviceEventType
      final case class SensorUpdated(reading: Option[Double]) extends DeviceEventType
    }

    sealed trait UserEventType

    object UserEventType {
      final case class UserPurchase(item: String, price: Double) extends UserEventType
      case object UserAccountCreated                             extends UserEventType
    }
  }

  object DeGoesSolution {

    final case class Event[+A](id: Int, time: Instant, payload: A)

    final case class UserEvent(userName: String, userEventType: UserEventType)
    sealed trait UserEventType
    object UserEventType {
      final case class Purchase(item: String, price: Double) extends UserEventType
      final case object AccountCreated extends  UserEventType
    }

    final case class DeviceEvent(deviceId: Int, deviceEventType: DeviceEventType)
    sealed trait DeviceEventType
    object DeviceEventType {
      final case class SensorUpdated(reading: Option[Double]) extends DeviceEventType
      case object DeviceActivated extends  DeviceEventType
    }
  }

}

/**
 * DOCUMENT EDITING - EXERCISE SET 4
 *
 * Consider a web application that allows users to edit and store documents
 * of some type (which is not relevant for these exercises).
 */
object documents {
  final case class UserId(identifier: String)
  final case class DocId(identifier: String)
  final case class DocContent(body: String)

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, create a simplified but somewhat
   * realistic model of a Document.
   */
  type Document

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create a model of the access
   * type that a given user might have with respect to a document. For example,
   * some users might have read-only permission on a document.
   */
  type AccessType

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create a model of the
   * permissions that a user has on a set of documents they have access to.
   * Do not store the document contents themselves in this model.
   */
  type DocPermissions
}

/**
 * BANKING - EXERCISE SET 5
 *
 * Consider a banking application that allows users to hold and transfer money.
 */
object bank {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, develop a model of a customer at a bank.
   */
  type Customer

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of an account
   * type. For example, one account type allows the user to write checks
   * against a given currency. Another account type allows the user to earn
   * interest at a given rate for the holdings in a given currency.
   */
  type AccountType

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a bank
   * account, including details on the type of bank account, holdings, customer
   * who owns the bank account, and customers who have access to the bank account.
   */
  type Account
}

/**
 * STOCK PORTFOLIO - GRADUATION PROJECT
 *
 * Consider a web application that allows users to manage their portfolio of investments.
 */
object portfolio {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * exchange. Ensure there exist values for NASDAQ and NYSE.
   */
  type Exchange

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of a currency
   * type.
   */
  type CurrencyType

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * symbol. Ensure there exists a value for Apple's stock (APPL).
   */
  type StockSymbol

  /**
   * EXERCISE 4
   *
   * Using only sealed traits and case classes, develop a model of a portfolio
   * held by a user of the web application.
   */
  type Portfolio

  /**
   * EXERCISE 5
   *
   * Using only sealed traits and case classes, develop a model of a user of
   * the web application.
   */
  type User

  /**
   * EXERCISE 6
   *
   * Using only sealed traits and case classes, develop a model of a trade type.
   * Example trade types might include Buy and Sell.
   */
  type TradeType

  /**
   * EXERCISE 7
   *
   * Using only sealed traits and case classes, develop a model of a trade,
   * which involves a particular trade type of a specific stock symbol at
   * specific prices.
   */
  type Trade
}
