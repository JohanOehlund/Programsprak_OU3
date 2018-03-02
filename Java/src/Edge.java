public class Edge {



    private Vertex frmVert;
    private Vertex toVert;
    private Object weight;

    public Edge(Vertex frmVert, Vertex toVert, Object weight) {
        this.frmVert = frmVert;
        this.toVert = toVert;
        this.weight = weight;
    }

    public Object getWeight() {
        return weight;
    }

    public Vertex getFrmVert() {
        return frmVert;
    }

    public Vertex getToVert() {
        return toVert;
    }
}
