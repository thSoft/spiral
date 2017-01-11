package hu.thsoft.spiral.examples

import firebase.Firebase
import hu.thsoft.spiral.data.FirebaseDataStore

object Database {

  lazy val database = Firebase.app().database()

  def dataStore(path: String) = new FirebaseDataStore(database, database.ref(path))

}
