class IntSequenceTest {
  public static void main (String argv[]) {
    String implementation = argv[0];
    int appends  = Integer.parseInt(argv[1]);
    int prepends = Integer.parseInt(argv[2]);
    IntSequence seq = null;
    if (implementation.equals("list")) {
      seq = new ListIntSequence();
    } else if (implementation.equals("array")) {
      seq = new ArrayIntSequence();
    }
    for (int i = 0; i < appends; i++) {
      seq.append(i);
    }
    for (int i = 0; i < prepends; i++) {
      seq.prepend(i);
    }
    int len = seq.size();
    for (int i = 0; i < len; i++) {
      System.out.print(seq.get(i) + " ");
    }
    System.out.println();
  }
}
