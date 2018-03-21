public class Weight implements WeightInterface, Cloneable {
    public Object wt;
    public Weight(){

    }

    public Weight(Object wt){
        this.wt=wt;
    }

    @Override
    public Object getWT() {
        return this.wt;
    }

    @Override
    public void resetWT() {

    }

    @Override
    public void add(Object wt){
        this.wt=wt;
    }

    @Override
    public boolean compare(Object wt) {
        return this.wt.equals(wt);
    }

    protected Weight clone() throws CloneNotSupportedException {
        return (Weight) super.clone();
    }
}
