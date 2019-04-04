package org.ferpin.expertsystem;

import org.jpl7.Atom;
import org.jpl7.Compound;
import org.jpl7.Query;
import org.jpl7.Term;

public class PrologPuppeteer {
  public static boolean run(String clause) {
    Query q = new Query(new Compound(clause));
    return q.hasSolution();
  }

  public static boolean run(String predicateName, String atom) {
    Term goal = new Compound(predicateName, new Term[] {new Atom(atom)});
    Query q = new Query(goal);

    return q.hasNext();
  }

  public static boolean run(String predicateName, String[] atoms) {
    Term[] terms = new Term[atoms.length];
    for (int i = 0; i < atoms.length; i++) {
      terms[i] = new Atom(atoms[i]);
    }
    Query predicate = new Query(predicateName, terms);
    return predicate.hasSolution();
  }

  public static boolean consult(String filePath) {
    Query consult = new Query("consult", new Term[] {new Atom(filePath)});
    return consult.hasSolution();
  }
}
