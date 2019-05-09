package org.ferpin.expertsystem;

import java.util.ArrayList;
import java.util.Map;
import org.jpl7.Atom;
import org.jpl7.Compound;
import org.jpl7.Query;
import org.jpl7.Term;

public class PrologPuppeteer {
  public static ArrayList<String> query(String clause,  String ...atoms) {
    ArrayList<String> queryResult = new ArrayList<>();
    String query = clause + "(";
    if (atoms.length != 0) {
      for (int i = 0; i < atoms.length; i++) {
        query += atoms[i];
        query += ", ";
      }
    }
    query+= " X).";

    System.out.println(query);

    for (Map m : new Query(query)) {
      System.out.println("Resultado: " + m.get("X"));
      queryResult.add(m.get("X") + "");
    }
    System.out.println(queryResult);
    return queryResult;

  }

  public static void simpleQuery(String query) {
    new Query(query);
  }

  public static void simpleQuery2(String query) {
    new Query(query).hasNext();
  }

  public static boolean run(String clause) {
    Query q = new Query(new Compound(clause));
    return q.hasSolution();
  }

  public static boolean run(String clause, String atom) {
    Term goal = new Compound(clause, new Term[] {new Atom(atom)});
    Query q = new Query(goal);

    return q.hasNext();
  }

  public static boolean run(String clause, String[] atoms) {
    Term[] terms = new Term[atoms.length];
    for (int i = 0; i < atoms.length; i++) {
      terms[i] = new Atom(atoms[i]);
    }
    Query predicate = new Query(clause, terms);
    return predicate.hasSolution();
  }

  public static boolean consult(String filePath) {
    Query consult = new Query("consult", new Term[] {new Atom(filePath)});
    return consult.hasSolution();
  }
}
