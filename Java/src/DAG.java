import java.lang.reflect.Method;
import java.util.*;

public class DAG {

    private int identifier = 1;
    private ArrayList<Vertex> vertices = new ArrayList<>();
    private ArrayList<Edge> edges = new ArrayList<>();
    private HashMap<Integer, Integer> inDegrees = new HashMap<>();
    private ArrayList<ArrayList<Integer>> paths = new ArrayList<>();


    public DAG() {

    }

    public int add_vertex(Weight weight) {
        if (correctType(weight)){
            Vertex newVert = new Vertex(weight, identifier);
            vertices.add(newVert);
        }else{
            return -1;
        }

        return identifier++;
    }

    public void add_edge(int frmVertID, int toVertID, Weight weight) {
        if (!correctType(weight)){
            return;
        }

        Edge newEdge = new Edge(frmVertID, toVertID, weight);
        if (identifier > (newEdge.getFrmVertID()) && identifier > (newEdge.getToVertID()) &&
                newEdge.getFrmVertID() > 0 && newEdge.getToVertID() > 0) {

            edges.add(newEdge);
            if (topological_ordering().isEmpty()) {
                System.err.println("Cannot add egde from " + frmVertID + " to "
                        + toVertID + " because it creates a cycle!");
                edges.remove(newEdge);
            }

        } else {
            System.err.println("Cannot add egde from " + frmVertID + " to " + toVertID);
        }

    }

    private boolean correctType(Weight weight){
        for (Vertex vert:vertices) {
            if(vert.getWeight().getClass()!=weight.getClass()){
                System.out.println("Invalid weightClass: "+weight.getClass());
                return false;
            }
        }
        for (Edge edge:edges) {
            if(edge.getWeight().getClass()!=weight.getClass()){
                System.out.println("Invalid weightClass: "+weight.getClass());
                return false;
            }
        }
        return true;
    }




    private void getInDegrees(ArrayList<Vertex> vertices, ArrayList<Edge> edges) {

        inDegrees.clear();

        for (int i = 0; i < vertices.size(); i++) {

            int tempID = vertices.get(i).getIdentifier();
            int tempIn = 0;

            for (int j = 0; j < edges.size(); j++) {
                if (edges.get(j).getToVertID() == tempID) {
                    tempIn++;
                }
            }
            inDegrees.put(tempID, tempIn);
        }
    }

    public ArrayList<ArrayList<Integer>> getPaths() {
        return paths;
    }

    public void print_paths() {
        for (int i = 0; i < paths.size(); i++) {

            ArrayList<Integer> temp = paths.get(i);
            for (int j = 0; j < temp.size() ; j++) {
                System.out.println("ID: "+temp.get(j));
            }
            System.out.println("");
        }
    }

    public ArrayList<Integer> topological_ordering() {

        ArrayList<Integer> returnList = new ArrayList<>();
        ArrayList<Vertex> copyVerts = (ArrayList<Vertex>) vertices.clone();
        ArrayList<Edge> copyEdges = (ArrayList<Edge>) edges.clone();

        getInDegrees(copyVerts, copyEdges);

        while (!inDegrees.isEmpty()) {

            int loops = 0;
            boolean cycle = true;
            for (Integer vertID : inDegrees.keySet()) {
                if (inDegrees.get(vertID) == 0) {
                    cycle = false;
                    int size = copyEdges.size();
                    for (int i = 0; i < size; i++) {
                        Edge e = copyEdges.get(i-loops);
                        if (e.getFrmVertID() == vertID) {
                            copyEdges.remove(i-loops);
                            loops++;
                        }
                    }

                    loops=0;
                    returnList.add(vertID);

                    for (int i=0;i<copyVerts.size();i++) {
                        Vertex tempV=copyVerts.get(i);
                        if (tempV.getIdentifier()==vertID)
                            copyVerts.remove(i);
                    }
                }
            }
            if(cycle){
                returnList.clear();
                return returnList;
            }
            getInDegrees(copyVerts, copyEdges);
        }
        return returnList;
    }


    public Weight weightOfLongestPath(int fromID,int toID,Method f,Method g,Object main) throws Exception {

        findPaths(fromID,toID,new ArrayList<>());
        ArrayList<Weight> pathWeights = new ArrayList<>();
        for (ArrayList<Integer> path:paths) {

            Weight test=Weight.class.cast(vertices.get(0).getWeight());
            Weight a = test.clone();
            a.resetWT();


            for (int i = 0; i < path.size(); i++) {
                int vertID = path.get(i);
                for (Vertex vert:vertices) {
                    if (vert.getIdentifier()==vertID){
                        a.add(((Weight) f.invoke(main,vert)).getWT());

                    }
                }
                if(i < path.size()-1) {
                    for (Edge e : edges) {
                        if (e.getToVertID() == path.get(i+1) &&e.getFrmVertID() == vertID) {
                            a.add(((Weight) g.invoke(main,e)).getWT());
                        }
                    }
                }
            }
            pathWeights.add(a);
            System.out.println(a.getWT());
        }
        return getMaxWeight(pathWeights);
    }

    private Weight getMaxWeight(ArrayList<Weight> pathWeights) {

        Weight currHighWeight = null;

        for (Weight w:pathWeights) {
            if (currHighWeight == null) {
                currHighWeight = w;
            }
            else {
                if(currHighWeight.compare(w.getWT())) {
                    currHighWeight = w;
                }
            }
        }
        return currHighWeight;
    }


    public void findPaths(int currentID, int toID, ArrayList<Integer> path) {

        ArrayList<Integer> neighbours = getNeighbours(currentID);

        path.add(currentID);

        for (Integer neighbour:neighbours) {

            if(neighbour == toID) {
                path.add(neighbour);
                paths.add((ArrayList<Integer>)path.clone());
                path.clear();
                return;
            }
            findPaths(neighbour, toID, (ArrayList<Integer>) path.clone());
        }
        if (currentID == toID) {
            path.add(currentID);
            paths.add((ArrayList<Integer>) path.clone());
            path.clear();
        }
    }

    public ArrayList<Integer> getNeighbours(int id) {

        ArrayList<Integer> neighbours = new ArrayList<>();
        for (Edge e:edges) {
            if(e.getFrmVertID() == id) {
                neighbours.add(e.getToVertID());
            }
        }
        return neighbours;
    }


    public void print_inDegrees() {
        inDegrees.forEach((k, v) -> System.out.println("VertID: " + k + " Deg: " + v));
    }


    public void print_vertices() {
        for (int i = 0; i < vertices.size(); i++) {
            System.out.println(vertices.get(i).getWeight().getWT());
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
}




