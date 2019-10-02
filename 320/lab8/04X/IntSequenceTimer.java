class IntSequenceTimer {

  public static void main (String argv[]) {
    String implementation = argv[0];
    int repetitions = Integer.parseInt(argv[1]);
    int appends = Integer.parseInt(argv[2]);
    int prepends = Integer.parseInt(argv[3]);
    int reads = Integer.parseInt(argv[4]);
    for (; repetitions > 0; repetitions--) {
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
      // read from the middle!
      for (int i = 0; i < reads; i++) {
        seq.get(len/2);
      }
    }
  }
}
