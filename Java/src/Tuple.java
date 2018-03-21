public class Tuple {
    private int wt_int;
    private String wt_string;

    public Tuple(int wt_int, String wt_string){
        this.wt_int=wt_int;
        this.wt_string=wt_string;
    }


    public int getWt_int() {
        return wt_int;
    }

    public void setWt_int(int wt_int) {
        this.wt_int = wt_int;
    }

    public String getWt_string() {
        return wt_string;
    }

    public void setWt_string(String wt_string) {
        this.wt_string = wt_string;
    }
}
