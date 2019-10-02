class Example {

  static int f(int a,int b) {
    return a*b/b;
  }

  static public void main(String[] argv) {
    int a = Integer.parseInt(argv[0]);
    int b = Integer.parseInt(argv[1]);
    int c = f(a,b);
    System.out.println("a = " + a + " b = " + b + " c = " + c);
  }
}

