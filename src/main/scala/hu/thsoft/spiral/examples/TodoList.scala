package hu.thsoft.spiral.examples

import hu.thsoft.firebase.Firebase
import hu.thsoft.spiral.data._

object TodoList {

  val data = new ListData(new FirebaseDataStore(new Firebase("https://thsoft.firebaseio.com/spiral/examples/todoList")))(new TodoData(_))

}

class TodoReferenceData(dataStore: DataStore, sourceTodoData: TodoData) extends ReferenceData[TodoData](dataStore)(new TodoData(_)) {

  def scope = TodoList.data.changed.map(todoDatas => todoDatas.filter(_ != sourceTodoData))

  def getDisplayedName(referred: TodoData) = referred.name.data.changed

}

class TodoData(dataStore: DataStore) extends RecordData(dataStore, "Todo") {

  val name = field("Name", new StringData(_))

  val completed = field("Completed", new BooleanData(_))

  val blockedBy = field("Blocked by", new TodoReferenceData(_, this))

  def fields = Seq(name, completed, blockedBy)

}