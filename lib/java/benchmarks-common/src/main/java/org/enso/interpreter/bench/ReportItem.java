package org.enso.interpreter.bench;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.OptionalDouble;
import org.openjdk.jmh.util.Statistics;

/** Contains historic results for a single benchmark identified by label. */
@XmlRootElement
public class ReportItem {

  public static class MeasurementStatistics {
    private double error50;
    private double error95;

    public MeasurementStatistics() {
      this.error50 = Double.NaN;
      this.error95 = Double.NaN;
    }

    public MeasurementStatistics(double error50, double error95) {
      this.error50 = error50;
      this.error95 = error95;
    }

    public double getError50() {
      return error50;
    }

    public double getError95() {
      return error95;
    }

    public void setError50(double error50) {
      this.error50 = error50;
    }

    public void setError95(double error95) {
      this.error95 = error95;
    }

    public static MeasurementStatistics from(Statistics stats) {
      return new MeasurementStatistics(stats.getMeanErrorAt(0.5), stats.getMeanErrorAt(0.95));
    }
  }

  private String label;

  private List<Double> scores;
  private List<MeasurementStatistics> statistics;

  public ReportItem() {}

  public ReportItem(String label, List<Double> scores) {
    this.label = label;
    this.scores = scores;
    this.statistics = emptyListOfLength(scores.size());
  }

  public ReportItem(String label, List<Double> scores, List<MeasurementStatistics> statistics) {
    this.label = label;
    this.scores = scores;
    this.statistics = statistics;
  }

  @XmlElement
  public String getLabel() {
    return label;
  }

  public void setLabel(String label) {
    this.label = label;
  }

  @XmlElementWrapper(name = "scores")
  @XmlElement(name = "score")
  public List<Double> getScores() {
    return scores;
  }

  @XmlElementWrapper(name = "stats")
  @XmlElement(name = "stat")
  public List<MeasurementStatistics> getStatistics() {
    return statistics;
  }

  public void setScores(List<Double> scores) {
    if (scores == null) scores = new ArrayList<>();
    this.scores = scores;
  }

  public void setStatistics(List<MeasurementStatistics> statistics) {
    if (statistics == null) statistics = new ArrayList<>();
    this.statistics = statistics;
  }

  /**
   * Registers a new score for this item.
   *
   * @param score Score to register.
   */
  public void addScore(double score, MeasurementStatistics stats) {
    if (scores == null) {
      scores = new ArrayList<>();
    }
    if (statistics == null) {
      statistics = emptyListOfLength(scores.size());
    }
    scores.add(score);
    statistics.add(stats);
  }

  private static <T> List<T> emptyListOfLength(int length) {
    List<T> list = new ArrayList<>(length);
    for (int i = 0; i < length; i++) {
      list.add(null);
    }
    return list;
  }

  /**
   * @return The best (lowest) historic result for this benchmark.
   */
  @XmlTransient
  public Optional<Double> getBestScore() {
    OptionalDouble min = getScores().stream().mapToDouble(s -> s).min();
    return min.isPresent() ? Optional.of(min.getAsDouble()) : Optional.empty();
  }
}
