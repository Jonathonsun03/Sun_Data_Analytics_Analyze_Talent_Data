-- One row per selected transcript line, with canonical chat payment metadata.
SELECT
  keys.transcript_line_id,
  chat.message_type,
  chat.paid_amount_text,
  chat.paid_amount_value,
  chat.paid_currency
FROM qualitative_loader_chat_keys keys
LEFT JOIN text.chat_messages chat
  ON keys.source_record_key = chat.message_key;
