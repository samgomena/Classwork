import java.util.Arrays;

class Example {
  public static void main(String argv[]) {
    if (argv.length <= 1)
      usage();
    int n = 0;
    try {
      n = Integer.parseInt(argv[0]);
    } catch (NumberFormatException e) {
      usage();
    }
    for (int i = 0; i < n; i++)
      // System.out.println(i +  ": " + Arrays.toString(argv));
      System.out.println(i +  ": " + printCLArgs(argv));
  }

  private static void usage() {
    System.err.println("usage: java Example count strings");
    System.exit(1);
  }

  private static String printCLArgs(String arr[]) {
    String ret = "";
    for (int i = 1; i < arr.length; ++i) {
      ret += arr[i];
      ret += " ";
    }

    return ret;
  }
}


