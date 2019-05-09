package org.ferpin.expertsystem.controller;

import javafx.fxml.FXML;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javax.swing.GroupLayout.Alignment;
import org.ferpin.expertsystem.Interpreter;
import org.ferpin.expertsystem.MessageType;
import org.ferpin.expertsystem.PrologPuppeteer;

public class MessageViewController {
  @FXML
  private TextField txtMessage;

  @FXML
  private Button btnSend;

  @FXML
  private VBox messagesBox;

  @FXML
  private HBox hBoxMessage;

  @FXML
  private ScrollPane scrollPane;


  public void handleSend() {
    String message = txtMessage.getText();
    HBox hBox = new HBox(new Label(message));
    ((Label)hBox.getChildren().get(0)).setText(message);
    ((Label)hBox.getChildren().get(0)).setWrapText(true);
    hBox.getChildren().get(0).getStyleClass().add("card");
    hBox.getChildren().get(0).setStyle("-fx-background-color: #7ff571;");
    hBox.setAlignment(Pos.CENTER_RIGHT);
    messagesBox.setPrefHeight(messagesBox.getHeight() + hBox.getHeight());
    messagesBox.getChildren().add(hBox);
    String array[] = message.split(" ");
//    if ( (array.length / 5) > 1) {
//      int toAdd = array.length / 6 * 32;
//      System.out.println("To add " + toAdd);
//      hBox.setPrefHeight(hBox.getPrefHeight() + toAdd);
//      hBox.setMinHeight(hBox.getPrefHeight() + toAdd);
////      messagesBox.setPrefHeight(messagesBox.getHeight() + toAdd);
//    }

    txtMessage.setText("");
    System.out.println(message);

    answer(Interpreter.processMessage(message));
//    PrologPuppeteer.query("ask");
  }

  public void answer(String message) {
    HBox hBox = new HBox(new Label(message));
    ((Label)hBox.getChildren().get(0)).setText(message);
    ((Label)hBox.getChildren().get(0)).setWrapText(true);
    hBox.getChildren().get(0).getStyleClass().add("card");
    hBox.getChildren().get(0).setStyle("-fx-background-color: #dbdbdb;");
    hBox.setAlignment(Pos.CENTER_LEFT);
    messagesBox.setPrefHeight(messagesBox.getHeight() + hBox.getHeight());
    messagesBox.getChildren().add(hBox);
    scrollPane.vvalueProperty().bind(messagesBox.heightProperty());
  }

  @FXML
  public void initialize() {
     Interpreter.setMessageType(MessageType.NAME);
    btnSend.defaultButtonProperty().bind(btnSend.focusedProperty());
    txtMessage.getParent().setOnKeyPressed(e ->
    {
      if (e.getCode() == KeyCode.ENTER && txtMessage.isFocused())
      {
        handleSend();
      }
    });

  }
}
