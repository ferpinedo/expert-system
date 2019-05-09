package org.ferpin.expertsystem;


import java.io.FileInputStream;
import java.net.URL;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Parent;
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
    PrologPuppeteer.simpleQuery("clean");
    System.out.println( "Consult: " + (PrologPuppeteer.consult("F:\\Repos\\expert-system\\src\\main\\resources\\org\\ferpin\\expertsystem\\algorithm\\expert-system.pl") ? "True" : "False"));
    System.out.println("Read lines: " + PrologPuppeteer.run("loadRules", "F:\\Repos\\expert-system\\src\\main\\resources\\org\\ferpin\\expertsystem\\knowledge\\rulesEnfermedadesUno.txt"));
    PrologPuppeteer.run("initRules");

    primaryStage.setTitle("Sistema Experto");
    URL url = Main.class.getResource("view/MainView.fxml");
    FXMLLoader fxmlLoader = new FXMLLoader(url);
    Parent parent = fxmlLoader.load();

    Scene scene = new Scene(parent);

    primaryStage.setScene(scene);
    primaryStage.show();
  }
}
