public class Example {

  static class P {
    int a;
    P (int a) { this.a = a; } 
  }

  static void twiddle(P x, P y) {
    P z = x;
    x   = y;
    y   = z;
  }
  
  static void swizzle(P x, P y) {
    int z = x.a;
    x.a   = y.a;
    y.a   = z;
  }

  static public void main(String argv[]) {
    P p0 = new P(0);
    P p1 = new P(1);
    System.out.println (p0.a + " " + p1.a);
    twiddle(p0, p1);
    System.out.println (p0.a + " " + p1.a);
    swizzle(p0, p1);
    System.out.println (p0.a + " " + p1.a);
  }
}

