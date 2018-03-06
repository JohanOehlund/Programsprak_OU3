import java.util.*;
import java.util.concurrent.Callable;

public class DAG {

    public int identifier = 1;
    public ArrayList<Vertex> vertices = new ArrayList<>();
    public ArrayList<Edge> edges = new ArrayList<>();
    private HashMap<Integer, Integer> inDegrees = new HashMap<>();

    public DAG() {

    }

    public int add_vertex(Object weight) {
        Vertex newVert = new Vertex(weight, identifier);
        vertices.add(newVert);
        return identifier++;
    }

    public void add_edge(int frmVertID, int toVertID, Object weight) {
        Edge newEdge = new Edge(frmVertID, toVertID, weight);
        if (identifier > (newEdge.getFrmVertID()) && identifier > (newEdge.getToVertID()) &&
                newEdge.getFrmVertID() > 0 && newEdge.getToVertID() > 0) {

            edges.add(newEdge);
            if (topologicalSort().isEmpty()) {
                System.err.println("Cannot add egde from " + frmVertID + " to "
                        + toVertID + " because it creates a cycle!");
                edges.remove(newEdge);
            }

        } else {
            System.err.println("Cannot add egde from " + frmVertID + " to " + toVertID);
        }

    }

    /*public Object weight_of_longest_path(int from, int to, Callable<Integer> f,
                                       Callable<Integer> g) {

    }*/

    public ArrayList<Integer> topologicalSort() {

        ArrayList<Integer> returnList = new ArrayList<>();
        ArrayList<Vertex> copyVerts = new ArrayList<>(vertices);
        ArrayList<Edge> copyEdges = new ArrayList<>(edges);

        for (int i = 0; i < copyVerts.size(); i++) {

            if (!getInDegrees(copyVerts, copyEdges).containsValue(0)) {
                returnList.clear();
                return returnList;
            }

            for (Integer key : inDegrees.keySet()) {
                if (inDegrees.get(key) == 0) {
                    returnList.add(key);
                    for (int k = 0; k < copyEdges.size(); k++) {
                        Edge tempEdge = copyEdges.get(k);
                        if (tempEdge.getFrmVertID() == key) {
                            copyEdges.remove(k);
                        }
                    }
                    for (int l = 0; l < copyVerts.size(); l++) {
                        Vertex tempVert2 = copyVerts.get(l);
                        if (tempVert2.getIdentifier() == key) {
                            copyVerts.remove(l);
                        }
                    }

                }
            }
            i = 0;
        }

        while (copyEdges.size() == 0 && !copyVerts.isEmpty()) {
            returnList.add(copyVerts.get(0).getIdentifier());
            copyVerts.remove(0);
        }
        //print_inDegrees();
        return returnList;
    }

    public HashMap<Integer, Integer> getInDegrees(ArrayList<Vertex> vertices, ArrayList<Edge> edges) {

        inDegrees.clear();

        for (int i = 0; i < vertices.size(); i++) {

            Vertex tempVert = vertices.get(i);
            int tempID = tempVert.getIdentifier();
            int tempIn = 0;


            for (int j = 0; j < edges.size(); j++) {
                if (edges.get(j).getToVertID() == tempID) {
                    tempIn++;
                }
            }
            inDegrees.put(tempID, tempIn);
        }

        return inDegrees;
    }

    private void print_inDegrees() {
        inDegrees.forEach((k, v) -> System.out.println("VertID: " + k + " Deg: " + v));
    }


    public void print_vertices() {
        for (int i = 0; i < vertices.size(); i++) {
            System.out.println(vertices.get(i).identifier);
        }
    }

    public void print_edges() {
        for (int i = 0; i < edges.size(); i++) {
            System.out.println("From: " + edges.get(i).getFrmVertID() + " To: " + edges.get(i).getToVertID() + " Weight: " +
                    edges.get(i).getWeight());
        }
    }

    public ArrayList<Vertex> getVertices() {
        return vertices;
    }

    public ArrayList<Edge> getEdges() {
        return edges;
    }

    public ArrayList<Integer> findPaths() {

        ArrayList<Integer> ret = new ArrayList<>();
        ArrayList<Integer> topList = topologicalSort();

        for (int i = 0; i < topList.size(); i++) {

        }
        return ret;
    }
}




