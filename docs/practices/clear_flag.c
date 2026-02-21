void main() {
    device target = radar("ally", "any", "any", "distance", @this, 1);
    ubind(target);
    ucontrol("flag", 0);
}