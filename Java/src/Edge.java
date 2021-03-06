/**
 * Author: Arvid, Johan
 * File: Edge.java
 * Created: 18-03-01
 * Description: A weighted edge between two vertice-IDs.
 */

public class Edge {

    private int frmVertID;
    private int toVertID;
    private Weight weight;

    public Edge(int frmVertID, int toVertID, Weight weight) {
        this.frmVertID = frmVertID;
        this.toVertID = toVertID;
        this.weight = weight;
    }

    public Weight getWeight() {
        return weight;
    }

    public int getFrmVertID() {
        return frmVertID;
    }

    public int getToVertID() {
        return toVertID;
    }
}
