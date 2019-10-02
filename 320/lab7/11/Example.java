public class Example {

  static public void main(String[] argv) {
    int a = Integer.parseInt(argv[0]);
    int b = a + 2000000000;
    if (a <= b)
	System.out.println(a + " <= " + b);
    else
	System.out.println(a + " > " + b);
  }
}
