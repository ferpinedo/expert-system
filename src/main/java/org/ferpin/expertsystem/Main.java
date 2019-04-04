package org.ferpin.expertsystem;


import java.io.FileInputStream;
import javafx.application.Application;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import org.jpl7.Atom;
import org.jpl7.Compound;
import org.jpl7.Query;
import org.jpl7.Term;

public class Main extends Application {
  public static void main(String[] args) {
    launch(args);
  }

  public void start(Stage primaryStage) throws Exception {
    System.out.println( "Consult: " + (PrologPuppeteer.consult("F:\\Repos\\expert-system\\src\\main\\resources\\org\\ferpin\\expertsystem\\algorithm\\expert-system.pl") ? "True" : "False"));
    System.out.println("Read lines: " + PrologPuppeteer.run("loadRules", "F:\\Repos\\expert-system\\src\\main\\resources\\org\\ferpin\\expertsystem\\knowledge\\rulesEnfermedadesUno.txt"));


    primaryStage.setTitle("Sistema Experto");

    VBox vBox = new VBox();
    Scene scene = new Scene(vBox);

    vBox.setAlignment(Pos.CENTER);
    vBox.setSpacing(10);
    vBox.setPadding(new Insets(10));

//    FileInputStream fileInputStream = new FileInputStream("resources/images/man.png");
//    Image image = new Image(fileInputStream);
//    ImageView imageView = new ImageView(image);
//    vBox.getChildren().add(imageView);

    Label label = new Label("¿Qué me quieres decir?");
    vBox.getChildren().add(label);

    TextField textField = new TextField();
    vBox.getChildren().add(textField);

    Button button = new Button("Enviar");
    vBox.getChildren().add(button);

    button.setOnAction(event -> {
      System.out.println(textField.getText());
//      textField.setText("");
//      label.setText("Gracias por tu participacion");
    });

    primaryStage.setScene(scene);
    primaryStage.show();
  }
}
