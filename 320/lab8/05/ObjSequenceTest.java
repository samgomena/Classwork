class ObjSequenceTest {

  public static void main (String argv[]) {
    String implementation = argv[0];
    int appends = Integer.parseInt(argv[1]);
    int prepends = Integer.parseInt(argv[2]);
    ObjSequence seq = null;
    if (implementation.equals("list")) {
      seq = new ListObjSequence();
    } else if (implementation.equals("array")) {
      seq = new ArrayObjSequence();
    }
    for (int i = 0; i < appends; i++) {
      seq.append("A");
    }
    for (int i = 0; i < prepends; i++) {
      seq.prepend(3.14);
    }
    int len = seq.size();
    for (int i = 0; i < len; i++) {
      System.out.print(seq.get(i) + " ");
    }
    System.out.println();
  }
}
