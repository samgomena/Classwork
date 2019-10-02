public class Example {

  static int f(int a) {
    int b;
    int c;
    if (a < 100)
      c = 10;
    else
      c = 20;
    return a + b + c;
  }

  static public void main(String[] argv) {
    int a = Integer.parseInt(argv[0]);
    int d = f(a);
    System.out.println("a = " + a + " d = " + d);
  }
}

