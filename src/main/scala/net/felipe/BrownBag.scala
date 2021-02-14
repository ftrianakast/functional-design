package net.felipe

/**
 * Consider a web email interface, which allows users to filter emails and direct them to specific folders based on custom criteria
 */
object BrownBag {

  /**
   * Consider a web email interface, which allows users to filter emails and direct them to specific folders based on custom criteria
   */
  object EmailFilteringDirectEncoding {
    final case class Address(emailAddress: String)
    final case class Email(sender: Address, to: List[Address], subject: String, body: String)

    final case class EmailFilter(matches: Email => Boolean) {
      self =>
      // Implement a set of orthogonal, expressive and composable binary operators
      /**
       * EXERCISE 1
       *
       * Add an "and" operator that will match an email if both the first and
       * the second email filter match the email.
       */
      def &&(that: EmailFilter): EmailFilter = EmailFilter { email =>
        self.matches(email) && that.matches(email)
      }

      /**
       * EXERCISE 2
       *
       * Add an "or" operator that will match an email if either the first or
       * the second email filter match the email.
       */
      def ||(that: EmailFilter): EmailFilter = EmailFilter { email =>
        self.matches(email) || that.matches(email)
      }

      /**
       * EXERCISE 3
       *
       * Add a "negate" operator that will match an email if this email filter
       * does NOT match an email.
       */
      def negation: EmailFilter = EmailFilter { email =>
          !self.matches(email)
      }
    }
    object EmailFilter {
      // Implement the set of most simple unary operators
      def senderIs(address: Address): EmailFilter = EmailFilter(_.sender == address)

      def recipientIs(address: Address): EmailFilter = EmailFilter(_.to.contains(address))

      def subjectContains(phrase: String): EmailFilter = EmailFilter(_.subject.contains(phrase))

      def bodyContains(phrase: String): EmailFilter = EmailFilter(_.body.contains(phrase))
    }

    /**
     * Make an email filter that looks for subjects that contain the word "discount",
     * bodies that contain the word "N95",
     * and which are NOT addressed to "john@doe.com". Build this filter up compositionally"
     */
    lazy val emailFilter1 = (EmailFilter.subjectContains("discount")
      && EmailFilter.bodyContains("N95")
      && (EmailFilter.recipientIs(Address("ohn@doe.com"))).negation)

  }

  object EmailFilteringDeclarativeEncoding {
  }

}
