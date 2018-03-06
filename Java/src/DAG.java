import java.util.*;
import java.util.concurrent.Callable;

public class DAG {

    public int identifier = 1;
    public ArrayList<Integer> vertices = new ArrayList<>();
    public ArrayList<Edge> edges = new ArrayList<>();
    private HashMap<Integer, Integer> inDegrees = new HashMap<>();
    private ArrayList<ArrayList<Integer>> ret = new ArrayList<>();

    public DAG() {

    }

    public int add_vertex(Object weight) {
        Vertex newVert = new Vertex(weight, identifier);
        vertices.add(newVert.identifier);
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



    public HashMap<Integer, Integer> getInDegrees(ArrayList<Integer> vertices, ArrayList<Edge> edges) {

        inDegrees.clear();

        for (int i = 0; i < vertices.size(); i++) {

            int tempID = vertices.get(i);
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

    public void print_inDegrees() {
        inDegrees.forEach((k, v) -> System.out.println("VertID: " + k + " Deg: " + v));
    }


    public void print_vertices() {
        for (int i = 0; i < vertices.size(); i++) {
            System.out.println(vertices.get(i));
        }
    }

    public void print_edges() {
        for (int i = 0; i < edges.size(); i++) {
            System.out.println("From: " + edges.get(i).getFrmVertID() + " To: " + edges.get(i).getToVertID() + " Weight: " +
                    edges.get(i).getWeight());
        }
    }

    public ArrayList<Integer> getVertices() {
        return vertices;
    }

    public ArrayList<Edge> getEdges() {
        return edges;
    }

    public void findPaths(int currentID, int toID, ArrayList<Integer> path) {

        ArrayList<Integer> neighbours = getNeighbours(currentID);

        path.add(currentID);

        for (Integer neighbour:neighbours) {


            if(neighbour == toID) {
                path.add(neighbour);
                ret.add((ArrayList<Integer>)path.clone());
                path.clear();
                return;
            }
            findPaths(neighbour, toID, (ArrayList<Integer>) path.clone());
        }
        if (currentID == toID) {
            path.add(currentID);
            ret.add((ArrayList<Integer>) path.clone());
            path.clear();
            return;
        }
    }

    public ArrayList<Integer> getNeighbours(int id) {

        ArrayList<Integer> ret = new ArrayList<>();
        for (Edge e:edges) {
            if(e.getFrmVertID() == id) {
                ret.add(e.getToVertID());
            }
        }
        return ret;
    }

    public ArrayList<ArrayList<Integer>> getRet() {
        return ret;
    }

    public void print_ret() {
        for (int i = 0; i < ret.size(); i++) {

            ArrayList<Integer> temp = ret.get(i);
            for (int j = 0; j < temp.size() ; j++) {
                System.out.println("ID: "+temp.get(j));
            }
            System.out.println("");
        }
    }

    public ArrayList<Integer> topologicalSort() {

        ArrayList<Integer> returnList = new ArrayList<>();
        ArrayList<Integer> copyVerts = new ArrayList<>(vertices);
        ArrayList<Edge> copyEdges = new ArrayList<>(edges);


        if (!getInDegrees(copyVerts, copyEdges).containsValue(0)) {
            returnList.clear();
            System.out.println("I if: "+copyEdges.size());
            return returnList;
        }
        while (inDegrees.size() > 0) {
            for (Integer vertID : inDegrees.keySet()) {
                if (inDegrees.get(vertID) == 0) {
                    returnList.add(vertID);
                    copyVerts.remove(vertID);

                    for (int i = 0; i < copyEdges.size(); i++) {
                        Edge e = copyEdges.get(i);
                        if (e.getFrmVertID() == vertID) {
                            copyEdges.remove(i);
                        }
                    }
                }
            }
            getInDegrees(copyVerts, copyEdges);

        }


        return returnList;
    }


    /*public ArrayList<Integer> topologicalSort() {

        ArrayList<Integer> returnList = new ArrayList<>();
        ArrayList<Vertex> copyVerts = new ArrayList<>(vertices);
        ArrayList<Edge> copyEdges = new ArrayList<>(edges);
        System.out.println("E "+edges.size());

        for (int i = 0; i < copyVerts.size(); i++) {

            if (!getInDegrees(copyVerts, copyEdges).containsValue(0)) {
                returnList.clear();
                System.out.println("I if: "+copyEdges.size());
                return returnList;
            }

            for (Integer key : inDegrees.keySet()) {
                if (inDegrees.get(key) == 0) {
                    returnList.add(key);


                    for (int k = 0; k < copyEdges.size(); k++) {
                        Edge tempEdge = copyEdges.get(k);
                        if (tempEdge.getFrmVertID() == key) {
                            copyEdges.remove(k);
                            System.out.println("första");
                        }
                    }


                    for (int l = 0; l < copyVerts.size(); l++) {
                        Vertex tempVert2 = copyVerts.get(l);
                        if (tempVert2.getIdentifier() == key) {
                            copyVerts.remove(l);
                            System.out.println("andra");

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
        System.out.println("erwgtershsrjrstjhrsthgtrsh");
        return returnList;
    }*/
}




