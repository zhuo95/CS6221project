package main.scala


import scala.collection.mutable
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout.BorderPane
import scalafx.stage.FileChooser
import scalafx.event.ActionEvent
import scalafx.Includes._
import scalafx.geometry.Orientation
import scala.io.Source


object GUI extends JFXApp {
  stage = new PrimaryStage {
    title = "Document Statistics"
    scene = new Scene(1000,800){
      //menu bar
      val menuBar = new MenuBar
      val fileMenu = new Menu("File")
      val newItem = new MenuItem("Open")
      val existItem = new MenuItem("Exit")
      fileMenu.items = List(newItem,existItem)
      menuBar.menus = List(fileMenu)
      val rootPane = new BorderPane
      rootPane.top = menuBar
      val button = new Button("Analyze")
      rootPane.bottom = button
      val tabPane = new TabPane

      //left and right split
      val leftSplit = new SplitPane()
      val rightSplit = new SplitPane()
      leftSplit.orientation = Orientation.Vertical
      val topSplit = new SplitPane()
      topSplit.items ++= List(leftSplit,rightSplit)
      topSplit.dividerPositions = 0.6

      //leftSplit items
      val topLeftBorder = new BorderPane()
      val bottomLeftBorder = new BorderPane()
      val input = new TextArea()
      topLeftBorder.center = input
      val commandLine = new TextField()
      val commandArea = new TextArea()
      commandArea.editable = false
      bottomLeftBorder.top = commandLine
      bottomLeftBorder.center = commandArea
      leftSplit.items ++= List(topLeftBorder,bottomLeftBorder)
      leftSplit.dividerPositions = 0.7
      val tab = new Tab()
      tab.content = topSplit

      //rightSplit items
      val showArea = new TextArea()
      showArea.editable = false;
      rightSplit.items ++= List(showArea)

      tabPane += tab
      rootPane.center = tabPane
      root =rootPane

      existItem.onAction = (e: ActionEvent) =>{
        sys.exit(0)
      }

      newItem.onAction = (e: ActionEvent) =>{
        val fileChooser = new FileChooser()
        val selectedFile = fileChooser.showOpenDialog(stage)
        if(selectedFile != null){
          val s = Source.fromFile(selectedFile.getAbsolutePath).mkString
          input.setText(s)
        }
      }

      commandLine.onAction = (e : ActionEvent) =>{
        if(commandLine.getText().equals("clear")){
          commandArea.setText("")
        }else
        if(commandLine.getText().equals("run")){
          var pre = commandArea.getText()
          commandArea.setText(pre+" run \n")
          showArea.setText(Statistics.numOfCharacter(input.getText()))
          var t = showArea.getText()
          showArea.setText(t+Statistics.dictionary(input.getText()))
          var t2 = showArea.getText()
          showArea.setText(t2+Statistics.numOfWord(input.getText()))

        }else if(commandLine.getText().startsWith("read:")){
          val p = commandLine.getText().substring(5)
          try{
            var pre = commandArea.getText()
            commandArea.setText(pre+" read \n")
            val l = Source.fromFile(p).mkString
            input.setText(l)
          } catch {
            case e: Exception =>
            val pre = commandArea.getText()
            commandArea.setText(pre+"read error \n")
          }
        } else{
          val pre = commandArea.getText()
          commandArea.setText(pre + " error \n")
        }
        commandLine.setText("")
      }


      button.onAction = (e:ActionEvent) =>{
        showArea.setText(Statistics.numOfCharacter(input.getText()))
        var t = showArea.getText()
        showArea.setText(t+Statistics.dictionary(input.getText()))
        var t2 = showArea.getText()
        showArea.setText(t2+Statistics.numOfWord(input.getText()))
      }

    }
  }

}

object Statistics {

  def numOfCharacter(text: String): String = {
    var count = 0
    var res :String ="Histogram of the characters \n ==================\n"
    var uppcaseText = text.toUpperCase();
    var map = new mutable.HashMap[Character,Int]()
    var i =0
    for( i  <- 0 to uppcaseText.length-1 ){
      var c = uppcaseText.charAt(i)
      if(map.contains(c)){
        var num :Int = map.getOrElse(c,0)
        map.put(c,num+1)
      }else{
        map.put(c,1)
      }
    }
    for(i <- 65 to 90 ){
      var c :Char = i.toChar
      if(map.contains(c)){
        count = count +1
        if(count==6){
          val nums:Int = map.getOrElse(c,0)
          res += c.toString + ":"+ nums+"\n"
          count =0
        }else{
          val nums:Int = map.getOrElse(c,0)
          res += c.toString + ":" + nums + "   "
        }
      }
    }
    res
  }

  def dictionary(text:String) :String ={
    var res = "\n  \n Alphabetical list \n ==================\n"
    var paras = text.split("\n")
    var words = Array("1")
    for(i <- 0 to paras.length-1){
      if(paras(i)!=""){
        var line = paras(i).split(" ")
        line.foreach{ (e:String) =>
          if(e!=""&& !e.equals("\"") && !e.equals(",") && !e.equals(".") && !e.equals("?") && !e.equals(";") && !e.equals("!"))
          words = words :+ e
        }
      }
    }
    //put into Hashset
    var map = new mutable.HashMap[Character,Array[String]]()
    for(i <- 0 to words.size-1){
      if(map.contains(words(i).charAt(0).toUpper)){
        var newArr = map.get(words(i).charAt(0).toUpper).getOrElse(null)
        if(!newArr.contains(words(i))){
          newArr = newArr :+ words(i)
          map.put(words(i).charAt(0).toUpper,newArr)
        }
      }else{
        val newArr = Array(words(i))
        map.put(words(i).charAt(0).toUpper,newArr)
      }
    }
    //print out dictionary
    for(i <- 65 to 90){
      var c :Character = i.toChar
      var temp = c.toString + ": "
        if(map.contains(c)){
        var s = map.get(c)
        var changedS =s.getOrElse(null)
        for(i <- 0 to changedS.length-1){
          var w = changedS(i)
          temp += w + " "
        }
      }
      res += temp +"\n"
    }
    res
  }

  def numOfWord(text:String) :String ={
    var res = "\n Histogram of the words \n ==================\n"
    var map = new mutable.HashMap[String,Int]()
    var paras = text.split("\n")
    var words = Array("1")
    for(i <- 0 to paras.length-1){
      if(paras(i)!="") {
        var line = paras(i).split(" ")
        line.foreach { (e: String) =>
          if(e!=""&& !e.equals("\"") && !e.equals(",") && !e.equals(".") && !e.equals("?") && !e.equals(";") && !e.equals("!"))
          words = words :+ e
        }
      }
    }
    var i = 0
    for(i <- 0 to words.size-1){
      if(map.contains(words(i))){
        var num :Int = map.getOrElse(words(i),0)
        map.put(words(i),num+1)
      }else{
        map.put(words(i),1)
      }
    }
    map.foreach{ (e: (String,Int)) =>
      if(!e._1.equals("1"))
        res += e._1 + " : "+ e._2 + "\n"
    }

    res += "\n The top three words  \n ==================\n "
    var top1 ,top2 , top3 =0
    var res1,res2,res3 =""
    //top1
    map.foreach{(e: (String,Int)) =>
      if(e._2 > top1 && !e._1.equals("1")){
        res1 = e._1
        top1 = e._2
      }
    }

    //top2
    if(res3==""){
      map.foreach{(e: (String,Int)) =>
        if(e._2 > top2 && e._2<=top1 && !e._1.equals(res1) && !e._1.equals("1") ){
          res2 = e._1
          top2 = e._2
        }
      }
    }

    //top3
    if(res3==""){
      map.foreach{(e: (String,Int)) =>
        if( e._2 > top3 && e._2<=top2 && !e._1.equals(res2) && !e._1.equals("1") && !e._1.equals(res1) ){
          res3 = e._1
          top3 = e._2
        }
      }
    }

    res += res1+ "  "+res2+"  "+res3
    res
  }


}
