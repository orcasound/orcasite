defmodule Orcasite.Notifications.EmailTemplateTest do
  use ExUnit.Case
  
  describe "email templates" do
    test "new_detection template exists and is readable" do
      template_path = Path.expand("lib/orcasite/notifications/templates/new_detection.mjml.eex")
      assert File.exists?(template_path), "new_detection template file should exist"
      
      {:ok, content} = File.read(template_path)
      assert String.length(content) > 0, "template should not be empty"
      assert String.contains?(content, "<mjml>"), "template should contain MJML structure"
      assert String.contains?(content, "A new detection has been submitted"), "template should contain expected content"
    end
    
    test "confirmed_candidate template exists and is readable" do
      template_path = Path.expand("lib/orcasite/notifications/templates/confirmed_candidate.mjml.eex")
      assert File.exists?(template_path), "confirmed_candidate template file should exist"
      
      {:ok, content} = File.read(template_path)
      assert String.length(content) > 0, "template should not be empty"
      assert String.contains?(content, "<mjml>"), "template should contain MJML structure"
      assert String.contains?(content, "LISTEN NOW!"), "template should contain expected content"
    end
    
    test "templates contain required variables" do
      # Test new_detection template variables
      {:ok, new_detection_content} = File.read(Path.expand("lib/orcasite/notifications/templates/new_detection.mjml.eex"))
      assert String.contains?(new_detection_content, "{{ node_name }}"), "should contain node_name variable"
      assert String.contains?(new_detection_content, "{{ node }}"), "should contain node variable"
      assert String.contains?(new_detection_content, "meta[\"description\"]"), "should contain description variable"
      assert String.contains?(new_detection_content, "{{unsubscribe_url}}"), "should contain unsubscribe_url variable"
      
      # Test confirmed_candidate template variables
      {:ok, confirmed_candidate_content} = File.read(Path.expand("lib/orcasite/notifications/templates/confirmed_candidate.mjml.eex"))
      assert String.contains?(confirmed_candidate_content, "{{ node }}"), "should contain node variable"
      assert String.contains?(confirmed_candidate_content, "meta[\"message\"]"), "should contain message variable"
      assert String.contains?(confirmed_candidate_content, "{{ unsubscribe_url }}"), "should contain unsubscribe_url variable"
    end
  end
end