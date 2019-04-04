package org.ferpin.expertsystem;


import org.jpl7.Atom;
import org.jpl7.Compound;
import org.jpl7.Query;
import org.jpl7.Term;

public class Main {

  public static void main(String[] args) {
    System.out.println( "consult " + (consult("resources/org/ferpin/expertsystem/prolog/test.pl") ? "succeeded" : "failed"));

  }

  public static boolean belongs(String predicateName, String atom) {
    Term goal = new Compound(predicateName, new Term[] {new Atom(atom)};
    Query q = new Query(goal);

    return q.hasNext();
//    System.out.println(
//        "child_of(joe,ralf) is " +
//            ( q2.query() ? "provable" : "not provable" )
//    );
  }


  public static boolean belongs(String predicateName, String[] atoms) {
    Term[] terms = new Term[atoms.length];
    for (int i = 0; i < atoms.length; i++) {
      terms[i] = new Atom(atoms[i]);
    }
    Query predicate =
        new Query(
            predicateName,
            terms
        );
    return predicate.query();
  }

  public static boolean consult(String fileName) {
    Query q1 =
        new Query(
            "consult",
            new Term[] {new Atom(fileName)}
        );
    return q1.query();
  }

}
