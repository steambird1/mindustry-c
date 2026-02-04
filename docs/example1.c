// Example 1: Scatter shooting any air target

auto device scatter1;

void main() {
    int order = 1;
    device former = null;
    while (true) {
        device target = radar("flying", "any", "any", "distance", scatter1, order++);
        control("shootp", scatter1, target, 1);
        if (target == former) break;
    }
}