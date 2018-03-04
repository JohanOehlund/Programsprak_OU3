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
        if(topologicalSort(this).isEmpty()){
            System.err.println("Could not add edge, creates cycle!");
            edges.remove(newEdge);
        }
        return identifier++;
    }

    public ArrayList<Vertex> topologicalSort(DAG dag){
        ArrayList<Vertex> vertices = dag.getVertices();
        ArrayList<Edge> edges = dag.getEdges();

        for (Vertex vert:vertices) {

        }
        return vertices;
    }

    public ArrayList<Vertex> getVertices(){
        return vertices;
    }

    public ArrayList<Edge> getEdges(){
        return edges;
    }
}

