import java.lang.reflect.Method;
import java.util.ArrayList;

public class Main {



    public static void main(String[] var0) {

        DAG dag = new DAG();
        //###########################INT TEST###################################
        /*int id1 = dag.add_vertex(new Weight_INT(1));
        int id2 = dag.add_vertex(new Weight_INT(2));
        int id3 = dag.add_vertex(new Weight_INT(3));
        int id4 = dag.add_vertex(new Weight_INT(4));
        int id5 = dag.add_vertex(new Weight_INT(5));
        int id6 = dag.add_vertex(new Weight_INT(6));
        int id7 = dag.add_vertex(new Weight_INT(7));
        int id8 = dag.add_vertex(new Weight_INT(8));

        dag.add_edge(1,2,new Weight_INT(10));
        dag.add_edge(2,4,new Weight_INT(11));
        dag.add_edge(4,5,new Weight_INT(12));
        dag.add_edge(3,6,new Weight_INT(13));
        dag.add_edge(1,4,new Weight_INT(14));
        dag.add_edge(5,3,new Weight_INT(15));
        dag.add_edge(2,5,new Weight_INT(16));
        dag.add_edge(1,6,new Weight_INT(17));*/
        //###########################INT TEST###################################

        //###########################CHAR/STRING TEST###################################
        int id1 = dag.add_vertex(new Weight_String('a'));
        int id2 = dag.add_vertex(new Weight_String('b'));
        int id3 = dag.add_vertex(new Weight_String('c'));
        int id4 = dag.add_vertex(new Weight_String('d'));
        int id5 = dag.add_vertex(new Weight_String('e'));
        int id6 = dag.add_vertex(new Weight_String('f'));
        int id7 = dag.add_vertex(new Weight_String('g'));
        int id8 = dag.add_vertex(new Weight_String('h'));

        dag.add_edge(1,2,new Weight_String('i'));
        dag.add_edge(2,4,new Weight_String('j'));
        dag.add_edge(4,5,new Weight_String('k'));
        dag.add_edge(3,6,new Weight_String('l'));
        dag.add_edge(1,4,new Weight_String('m'));
        dag.add_edge(5,3,new Weight_String('n'));
        dag.add_edge(2,5,new Weight_String('o'));
        dag.add_edge(1,6,new Weight_String('p'));
        //###########################CHAR/STRING TEST###################################


        //###########################CHAR/STRING TEST###################################
        /*int id1 = dag.add_vertex(new Weight_Tuple(new Tuple(1,"a")));
        int id2 = dag.add_vertex(new Weight_Tuple(new Tuple(2,"b")));
        int id3 = dag.add_vertex(new Weight_Tuple(new Tuple(3,"c")));
        int id4 = dag.add_vertex(new Weight_Tuple(new Tuple(4,"d")));
        int id5 = dag.add_vertex(new Weight_Tuple(new Tuple(5,"e")));
        int id6 = dag.add_vertex(new Weight_Tuple(new Tuple(6,"f")));
        int id7 = dag.add_vertex(new Weight_Tuple(new Tuple(7,"g")));
        int id8 = dag.add_vertex(new Weight_Tuple(new Tuple(8,"h")));

        dag.add_edge(1,2,new Weight_Tuple(new Tuple(10,"i")));
        dag.add_edge(2,4,new Weight_Tuple(new Tuple(11,"j")));
        dag.add_edge(4,5,new Weight_Tuple(new Tuple(12,"k")));
        dag.add_edge(3,6,new Weight_Tuple(new Tuple(13,"l")));
        dag.add_edge(1,4,new Weight_Tuple(new Tuple(14,"m")));
        dag.add_edge(5,3,new Weight_Tuple(new Tuple(15,"n")));
        dag.add_edge(2,5,new Weight_Tuple(new Tuple(16,"o")));
        dag.add_edge(1,6,new Weight_Tuple(new Tuple(17,"p")));*/
        //###########################CHAR/STRING TEST###################################


        try {
            Method f = Main.class.getMethod("getVertWeight", Vertex.class);
            Method g = Main.class.getMethod("getEdgeWeight", Edge.class);
            Weight test = dag.weightOfLongestPath(1,6,f,g,new Main());
            System.out.println("Störst vikt: "+test.getWT());
        } catch (Exception e) {
            e.printStackTrace();
        }

    }


    public Weight getVertWeight(Vertex vertex) {
        return vertex.getWeight();
    }

    public Weight getEdgeWeight(Edge edge) {
        return edge.getWeight();
    }


}
