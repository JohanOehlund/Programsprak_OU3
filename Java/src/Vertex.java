
public class Vertex {

    private int identifier;
    private Weight weight;


    public Vertex(Weight weight, int identifier) {
        this.identifier = identifier;
        this.weight = weight;
    }

    public int getIdentifier() {
        return identifier;
    }

    public Weight getWeight() {
        return weight;
    }

}
