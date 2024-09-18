package entity;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Vertex {
    public String name;
    public double d = 0;
    public boolean visit = false;
    public int xCoordinate;
    public int yCoordinate;
    public List<Vertex> adj = new ArrayList<>();
    public Map<Vertex, Double> adjValues = new HashMap<>();

    public Vertex(String name, int xCoordinate, int yCoordinate) {
        this.name = name;
        this.xCoordinate = xCoordinate;
        this.yCoordinate = yCoordinate;
    }
}
