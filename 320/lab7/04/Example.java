import java.io.*;
import java.util.*;

class Example {
  public static void main(String argv[]) throws IOException {
    double[] ds     = readDoubleArray(argv[0]);
    double mean     = mean(ds);
    double variance = variance(ds,mean);
    double stdev    = Math.sqrt(variance);
    System.out.println("mean = " + mean + " variance = " + variance + " stdev = " + stdev);
  }

  private static double mean(double[] ds) {
    double sum = 0.0;
    for (double d : ds) 
      sum += d;
    return sum/ds.length;
  }

  private static double variance(double[] ds, double mean) {
    double sumsqdiff = 0.0;
    for (double d : ds)
      sumsqdiff += Math.pow(d-mean,2);
    return sumsqdiff/ds.length;
  }

  private static double[] readDoubleArray(String filename) throws IOException {
    FileReader f = new FileReader(filename);
    Scanner sc = new Scanner(f);
    int n = sc.nextInt();
    double[] ds = new double[n];
    for (int i = 0; i < n; i++)
      ds[i] = sc.nextDouble();
    f.close();
    return ds;
  }
}


