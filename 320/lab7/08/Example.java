import java.io.*;
import java.util.*;

class Example {

  static class P {
    double x;
    double y;
    P (double x, double y) { this.x = x; this.y = y; }
  }

  public static void main(String argv[]) {
    if(argv.length < 2) {
      System.out.println("Usage:\n\t<number>\n\t<filename>");
    }
    int lines = Integer.parseInt(argv[0]);
    String filename = argv[1];
    P[] pArr = new P[lines];

    for(int i = 0; i < pArr.length; ++i) {
      pArr[i] = new P(0, 0);
      System.out.println(pArr[i]);
    }
  } 

  private static String[] readFile(String filename) throws IOException {
    FileReader f = new FileReader(filename);
    Scanner sc = new Scanner(f);
    String line = sc.nextLine();

  }

}
