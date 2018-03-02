import java.util.ArrayList;

public class DAG {

    public int identifier = 1;
    public ArrayList<Vertex> vertices = new ArrayList<>();
    public ArrayList<Edge> edges = new ArrayList<>();

    public DAG() {

    }

    public int add_vertex(Object weight) {
        Vertex newVert = new Vertex(weight, identifier);
        vertices.add(newVert);
        return identifier++;
    }

    public int add_edge(Vertex frmVert, Vertex toVert, Object weight) {
        Edge newEdge = new Edge(frmVert, toVert, weight);
        edges.add(newEdge);
        return identifier++;
    }
}

