package ambiparse;

import clojure.lang.*;
import java.util.*;

public class Machine {
  // Essential state.
  public Indexed input;
  public String instr;
  public Object root;
  public Volatile graph;
  public Volatile queue;
  public Volatile buffered;
  public Volatile breaks; // Maps line minus one to index of previous newline.
  public int traveled; // Furthest index into the input examined.

  // Debug state.
  public boolean trace;
  public long fuel; // Steps to perform before giving up. Zero disables.
}
