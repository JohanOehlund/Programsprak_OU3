public class Edge {

    private int frmVertID;
    private int toVertID;
    private Object weight;

    public Edge(int frmVertID, int toVertID, Object weight) {
        this.frmVertID = frmVertID;
        this.toVertID = toVertID;
        this.weight = weight;
    }

    public Object getWeight() {
        return weight;
    }

    public int getFrmVertID() {
        return frmVertID;
    }

    public int getToVertID() {
        return toVertID;
    }
}
