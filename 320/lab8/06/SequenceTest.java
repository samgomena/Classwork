class SequenceTest {

  public static void main (String argv[]) {
    String implementation = argv[0];
    int appends = Integer.parseInt(argv[1]);
    int prepends = Integer.parseInt(argv[2]);
    Sequence<String> seq = null;
    if (implementation.equals("list")) {
      seq = new ListSequence<String>();
    } else if (implementation.equals("array")) {
      seq = new ArraySequence<String>();
    }
    for (int i = 0; i < appends; i++)  {
      seq.append("Z");
    }
    for (int i = 0; i < prepends; i++) {
      seq.prepend("A");
    }
    int len = seq.size();
    for (int i = 0; i < len; i++) {
      String s = seq.get(i);
      System.out.print(s + " ");
    }
    System.out.println();
  }
}
