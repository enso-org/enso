package org.enso.benchmarks.runner;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;
import jakarta.xml.bind.Unmarshaller;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;

/** Historic runs report. Supports XML serialization. */
@XmlRootElement
public class Report {

  private List<ReportItem> tests;

  public Report() {
    tests = new ArrayList<>();
  }

  public Report(List<ReportItem> tests) {
    this.tests = tests;
  }

  @XmlElementWrapper(name = "cases")
  @XmlElement(name = "case")
  public List<ReportItem> getTests() {
    return tests;
  }

  public void setTests(List<ReportItem> tests) {
    this.tests = tests;
  }

  /**
   * Finds a historic result by label.
   *
   * @param label name of the result to find.
   * @return Result for the given label, if found.
   */
  public Optional<ReportItem> findByLabel(String label) {
    return getTests().stream().filter(item -> item.getLabel().equals(label)).findFirst();
  }

  /**
   * Inserts a new report item for a given label.
   *
   * @param label name for which to allocate a report slot.
   * @return Item allocated for the label.
   */
  public ReportItem createByLabel(String label) {
    ReportItem newItem = new ReportItem(label, new ArrayList<>());
    getTests().add(newItem);
    return newItem;
  }

  /**
   * Finds or creates a new report item for a given label.
   *
   * @param label name for which an item is needed.
   * @return The report item for the label, guaranteed to be present in this Report.
   */
  public ReportItem findOrCreateByLabel(String label) {
    return findByLabel(label).orElseGet(() -> createByLabel(label));
  }

  /**
   * Reads a Report from XML file.
   *
   * @param file the file from which to read the report.
   * @return the Report read from given file.
   * @throws JAXBException when the file cannot be read or does not conform to the Report XML
   *     format.
   */
  public static Report readFromFile(File file) throws JAXBException {
    JAXBContext jaxbContext = JAXBContext.newInstance(Report.class);
    Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
    return (Report) unmarshaller.unmarshal(file);
  }

  /**
   * Serializes a report to an XML file.
   *
   * @param report Report to serialize.
   * @param file File to which the serialized report should be written.
   * @throws JAXBException when the file cannot be written to.
   */
  public static void writeToFile(Report report, File file) throws JAXBException {
    JAXBContext jaxbContext = JAXBContext.newInstance(Report.class);
    Marshaller marshaller = jaxbContext.createMarshaller();
    marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
    marshaller.marshal(report, file);
  }
}
