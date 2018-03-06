import java.util.ArrayList;

public class Vertex {

    public int identifier;
    private Object weight;


    public Vertex(Object weight, int identifier) {
        this.identifier = identifier;
        this.weight = weight;
    }

    public int getIdentifier() {
        return identifier;
    }

    public Object getWeight() {
        return weight;
    }

}
