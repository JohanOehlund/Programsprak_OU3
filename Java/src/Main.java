import java.util.ArrayList;

public class Main {



    public static void main(String[] var0) {

        ArrayList<Integer> topList = new ArrayList<>();
        DAG dag = new DAG();
        int id7 = dag.add_vertex(7);
        int id1 = dag.add_vertex(1);
        int id2 = dag.add_vertex(2);
        int id3 = dag.add_vertex(3);
        int id4 = dag.add_vertex(4);
        int id5 = dag.add_vertex(5);
        int id6 = dag.add_vertex(6);
        int id8 = dag.add_vertex(8);



        dag.add_edge(1,2,10);
        dag.add_edge(2,4,11);
        dag.add_edge(4,5,12);
        dag.add_edge(3,6,13);
        dag.add_edge(1,4,14);
        dag.add_edge(5,3,15);
        dag.add_edge(2,5,16);

        //dag.add_edge(5,1,16);


        topList = dag.topologicalSort();

        System.out.println("##### TOPSORT #####");
        for (int i = 0; i < topList.size(); i++) {
            System.out.println(topList.get(i));
        }
        System.out.println("###################");

        dag.findPaths();

        //dag.weight_of_longest_path(1, 5, dag.add_vertex(2), dag.add_vertex(4));

        //dag.print_vertices();
        //dag.print_edges();
    }





}
